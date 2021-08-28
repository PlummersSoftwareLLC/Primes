extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, TokenTree};
use quote::{quote, ToTokens};
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

fn extreme_reset(skip: usize) -> TokenStream {
    let index_range = 0..skip;

    // word reset statements
    let word_resets: Vec<_> = index_range.clone()
        .map(|idx| extreme_reset_word(skip, idx))
        .collect();

    let code = quote! {
        // whole chunks
        words.chunks_exact_mut(#skip).foreach(|chunk| {
            let slice = chunk;
            #(
                #word_resets
            )*
        });

        // remainder
        let remainder = words.chunks_exact_mut(#skip).into_remainder();
        // ??? how to dispatch lots of ifs ???
        #(
            let slice = remainder;
            if #index_range < remainder.len() {
                #word_resets
            }
        )*

        // restore original factor bit -- we have clobbered it, and it is the prime
        let factor_index = #skip / 2;
        let factor_word = factor_index / 64;
        let factor_bit = factor_index % 64;
        if let Some(w) = words.get_mut(factor_word) {
            *w &= !(1 << factor_bit);
        }
    };

    println!("Extreme reset: {}", code.to_string());

    code.into()
}

fn extreme_reset_word(skip: usize, word_idx: usize) -> proc_macro2::TokenStream {
    let masks = calculate_masks(skip, word_idx);
    let code = quote! {
        unsafe {
            let mut word = *slice.get_unchecked(#word_idx);
            #(
                *word |= #masks;
            )*
            *slice.get_unchecked_mut(#word_idx) = word;
        }
    };
    code
}
