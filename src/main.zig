const std = @import("std");
const stringzilla = @import("stringzilla");

pub fn main() !void {
    std.debug.print("StringZilla Zig Facade Test\n", .{});

    const haystack = "Hello, World!";
    const needle = "World";

    if (stringzilla.find(haystack, needle)) |pos| {
        std.debug.print("Found '{s}' at position {}\n", .{ needle, pos });
    } else {
        std.debug.print("Did not find '{s}'\n", .{needle});
    }

    const sum = stringzilla.bytesum(haystack);
    std.debug.print("Bytesum: {}\n", .{sum});

    const hash_val = stringzilla.hash(haystack);
    std.debug.print("Hash: {}\n", .{hash_val});

    const char_set = stringzilla.Byteset.from_bytes("aeiou");
    if (stringzilla.find_byteset(haystack, char_set)) |pos| {
        std.debug.print("First vowel at position {}\n", .{pos});
    }

    if (stringzilla.find_byte_from(haystack, "aeiou")) |pos| {
        std.debug.print("First vowel (using find_byte_from) at position {}\n", .{pos});
    }

    std.debug.print("All tests completed successfully!\n", .{});
}
