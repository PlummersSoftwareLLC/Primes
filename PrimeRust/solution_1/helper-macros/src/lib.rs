extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, TokenTree};
use quote::{format_ident, quote, ToTokens};
use syn::{
    self,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Expr, Ident, Token,
};

fn parse_as_usize(expr: &Expr) -> Option<usize> {
    if let Expr::Lit(syn::ExprLit {
        attrs: _,
        lit: syn::Lit::Int(i),
    }) = expr
    {
        Some(i.base10_parse().unwrap())
    } else {
        None
    }
}

struct GenericDispatchParams {
    match_var: Ident,
    start_value: Expr,
    increment_value: Expr,
    max_value: Expr,
    generic_function: Expr,
    fallback: Expr,
}

impl Parse for GenericDispatchParams {
    fn parse(input: ParseStream) -> Result<Self> {
        let match_var: Ident = input.parse()?;
        input.parse::<Token![,]>()?;

        let start_value: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        let increment_value: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        let max_value: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        let generic_function: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        let fallback: Expr = input.parse()?;
        Ok(Self {
            match_var,
            start_value,
            increment_value,
            max_value,
            generic_function,
            fallback,
        })
    }
}

/// Generic dispatcher that writes a big `match` statement from supplied parameters.
/// For the call,
/// ```ignore
/// generic_dispatch!(match_var, 3, 2, 7,
///        my_function::<N>(),
///        fallback()
/// );
/// ```
/// Resulting code is of the following form
/// ```
/// # fn my_function<const N:usize>() { }
/// # fn fallback() { }
/// # let match_var = 5;
/// match match_var {
///     3 => my_function::<3>(),
///     5 => my_function::<5>(),
///     7 => my_function::<7>(),
///     _ => fallback()
/// }
/// ```
/// Parameters are as follows:
/// - match variable
/// - start
/// - increment
/// - max
/// - generic function for a match
/// - fallback function for match_var outside of range
///
#[proc_macro]
pub fn generic_dispatch(input: TokenStream) -> TokenStream {
    let params = parse_macro_input!(input as GenericDispatchParams);

    let start = parse_as_usize(&params.start_value).expect("start value");
    let increment = parse_as_usize(&params.increment_value).expect("increment value");
    let max = parse_as_usize(&params.max_value).expect("max value");

    // create vector of values for start, start + inc, ... max
    let mut vals = vec![];
    let mut i = start;
    while i <= max {
        vals.push(i);
        i += increment;
    }

    // replace instances of "N" with literal corresponding to each value, using the `generic_function` as a template
    let ts = params.generic_function.clone().into_token_stream();
    let substituted = vals.iter().map(|&v| substitute_placeholder(ts.clone(), v));

    // write the match statement, and return it to the compiler
    let match_var = params.match_var;
    let fallback = params.fallback;
    let res = quote! {
        match #match_var {
            #(
                #vals => #substituted,
            )*
            #match_var => #fallback
        };
    };
    TokenStream::from(res)
}

/// Replace instances of the identifier `N` found in the supplied generic
/// expression, with the _literal_ `substitute_value` supplied. This allows
/// to replace `my_generic_function::<N>()` with `my_generic_function::<123>()`
/// for instance.
fn substitute_placeholder(
    ts: proc_macro2::TokenStream,
    substitute_value: usize,
) -> proc_macro2::TokenStream {
    ts.into_iter()
        .map(|tt| {
            match tt {
                TokenTree::Ident(id) => {
                    if id == "N" {
                        // replace with our literal value
                        TokenTree::Literal(Literal::usize_suffixed(substitute_value))
                    } else {
                        // unmodified
                        TokenTree::Ident(id)
                    }
                }
                TokenTree::Group(group) => {
                    let tsm = substitute_placeholder(group.stream(), substitute_value);
                    TokenTree::Group(Group::new(group.delimiter(), tsm))
                }
                _ => tt,
            }
        })
        .collect()
}

/// This produces a set of 64 single-bit masks for a given u64 word
/// and skip factor.
fn calculate_masks(skip: usize, word_idx: usize) -> Vec<u64> {
    let start = skip / 2;
    let first_idx = word_idx * u64::BITS as usize;
    let mut res = vec![];
    for bit in 0..u64::BITS {
        let idx = first_idx as isize + bit as isize;
        if (idx - start as isize) % skip as isize == 0 {
            let mask = 1 << bit;
            res.push(mask);
        }
    }
    res
}

/// Create an extreme reset function for a given `skip` factor, and name it according
/// to the supplied identifier, e.g. `extreme_reset_003(words: &mut [u64])`
fn extreme_reset_for_skip(skip: usize, function_name: Ident) -> proc_macro2::TokenStream {
    let index_range = 0..skip;

    // word reset statements
    let word_resets_chunk: Vec<_> = index_range
        .clone()
        .map(|idx| extreme_reset_word(quote! { chunk }, skip, idx))
        .collect();

    let word_resets_remainder: Vec<_> = index_range
        .clone()
        .map(|idx| extreme_reset_word(quote! {remainder}, skip, idx))
        .collect();

    // determine the offset of the first skip-size chunk we need
    // to touch, and proceed from there.
    let square_start = skip * skip / 2;
    let start_chunk_offset = square_start / 64 / skip * skip;

    let code = quote! {
        #[inline(never)]
        fn #function_name(words: &mut [u64]) {
            debug_assert!(
                #square_start < words.len() * 64,
                "square_start should be within the bounds of our array; check caller"
            );

            // whole chunks
            let mut chunks = words[#start_chunk_offset..].chunks_exact_mut(#skip);
            (&mut chunks).for_each(|chunk| {
                #(
                    #word_resets_chunk
                )*
            });

            // remainder; this seems inefficient, but appears to
            // work well enough
            let remainder = chunks.into_remainder();
            #(
                if #index_range < remainder.len() {
                    #word_resets_remainder
                }
            )*

            // restore original factor bit -- we have clobbered it, and it is the prime
            let factor_index = #skip / 2;
            let factor_word = factor_index / 64;
            let factor_bit = factor_index % 64;
            if let Some(w) = words.get_mut(factor_word) {
                *w &= !(1 << factor_bit);
            }
        }
    };
    //println!("Extreme reset: {}", code.to_string());
    code
}

/// Retrieves single word, then applies each single-bit mask in turn. When all masks
/// have been applied, write the word back to the slice.
fn extreme_reset_word(
    slice_expr: proc_macro2::TokenStream,
    skip: usize,
    word_idx: usize,
) -> proc_macro2::TokenStream {
    let single_bit_masks = calculate_masks(skip, word_idx);

    // emit no code when the given word has no masks applied to it
    if single_bit_masks.is_empty() {
        TokenStream::default();
    }

    // by value - load, apply, apply, ..., store.
    quote! {
        unsafe {
            let mut word = *#slice_expr.get_unchecked(#word_idx);
            #(
                word |= #single_bit_masks;
            )*
            *#slice_expr.get_unchecked_mut(#word_idx) = word;
        }
    }
}

struct ExtremeResetParams {
    match_var: Ident,
}

impl Parse for ExtremeResetParams {
    fn parse(input: ParseStream) -> Result<Self> {
        let match_var: Ident = input.parse()?;
        Ok(Self { match_var })
    }
}

/// This procedural macro writes the extreme reset functions and dispatcher.
/// The functions contain the code for each specific `skip` factor in [3,5,...129]
/// and are individually called based on the supplied `skip` factor.
#[proc_macro]
pub fn extreme_reset(input: TokenStream) -> TokenStream {
    let params = parse_macro_input!(input as ExtremeResetParams);

    // all odd numbers in [3,129]
    let last = 129_usize;
    let extreme_reset_vals: Vec<_> = (3..=last).filter(|skip| skip % 2 != 0).collect();

    // names for extreme reset functions: `extreme_reset_003`, `extreme_reset_005`, etc.
    let function_names: Vec<_> = extreme_reset_vals
        .iter()
        .map(|&skip| format_ident!("extreme_reset_{:03}", skip))
        .collect();

    // code for extreme reset functions
    let extreme_reset_codes: Vec<_> = extreme_reset_vals
        .iter()
        .zip(function_names.iter().cloned())
        .map(|(&skip, function_name)| extreme_reset_for_skip(skip, function_name))
        .collect();

    let match_var = params.match_var;
    let code = quote! {
        // code for functions
        #(
            #extreme_reset_codes
        )*

        // dispatcher to call specific function based on skip value
        match #match_var {
            #(
                #extreme_reset_vals => #function_names(words),
            )*
            _ => panic!("unexpected value")
        }
    };
    let ts: proc_macro2::TokenStream = code.into_iter().collect();
    TokenStream::from(ts)
}
