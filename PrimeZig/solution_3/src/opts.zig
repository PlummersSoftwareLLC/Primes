//! Zig no longer coerces anonymous struct values to named structs once they
//! have been created (for example when stored in the drag race layout tuple in
//! main.zig).  `coerce` performs that conversion explicitly, field by field,
//! recursing into nested option structs and filling in defaults.

const std = @import("std");

pub fn coerce(comptime T: type, comptime src: anytype) T {
    const S = @TypeOf(src);
    if (S == T) return src;

    switch (@typeInfo(T)) {
        .@"struct" => |info| if (@typeInfo(S) == .@"struct") {
            inline for (@typeInfo(S).@"struct".fields) |field| {
                if (!@hasField(T, field.name)) @compileError(@typeName(T) ++ " has no option " ++ field.name);
            }
            var result: T = undefined;
            inline for (info.fields) |field| {
                @field(result, field.name) = if (@hasField(S, field.name))
                    coerce(field.type, @field(src, field.name))
                else
                    field.defaultValue() orelse @compileError("missing option " ++ field.name);
            }
            return result;
        },
        .optional => |info| if (S != @TypeOf(null)) return coerce(info.child, src),
        else => {},
    }
    return src;
}
