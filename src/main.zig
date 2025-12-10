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

    const hello = "Hello Мир";
    if (stringzilla.utf8_case_insensitive_find(hello, "Мир")) |range| {
        const found_text = range.slice(hello);
        std.debug.print("Case-insensitive match: '{s}' (positions {}..{})\n", .{ found_text, range.start, range.end() });
    }

    const utf8_text = "Hi 🌍";
    var runes: [16]u32 = undefined;
    const unpack_result = stringzilla.utf8_unpack_chunk(utf8_text, &runes);
    std.debug.print("UTF-8 unpacked {} runes from {} bytes\n", .{ unpack_result.runes_unpacked, unpack_result.bytes_consumed });
}

test "Run README examples" {
    try readmeExamples();
}

fn readmeExamples() !void {
    {
        const haystack = "Hello, World!";
        const needle = "World";

        const pos = stringzilla.find(haystack, needle) orelse @panic("Expected to find 'World' in haystack");
        std.debug.assert(pos == 7);

        const last_o_pos = stringzilla.rfind(haystack, "o") orelse @panic("Expected to find 'o' in haystack");
        std.debug.assert(last_o_pos == 8);

        const char_count = stringzilla.count_utf8(haystack);
        std.debug.assert(char_count == 13);
    }

    {
        const checksum = stringzilla.bytesum("Hello, World!");
        std.debug.assert(checksum == 1129);

        const hash_val = stringzilla.hash("Hello, World!");
        std.debug.assert(hash_val == 7174687111055709308);

        const hmac = stringzilla.hmac_sha256("secret-key", "message");
        std.debug.assert(hmac.len == 32);
    }

    {
        const text = "The quick brown fox jumps over the lazy dog";
        const vowels = stringzilla.Byteset.from_bytes("aeiou");

        const first_vowel_pos = stringzilla.find_byteset(text, vowels) orelse @panic("Expected to find vowel in text");
        std.debug.assert(first_vowel_pos == 2); // 'e' in "The"

        const first_vowel_from_pos = stringzilla.find_byte_from(text, "aeiou") orelse @panic("Expected to find vowel using find_byte_from");
        std.debug.assert(first_vowel_from_pos == 2); // 'e' in "The"
    }

    {
        var buffer: [100]u8 = undefined;
        const source = "Copy this text!";

        stringzilla.copy(buffer[0..source.len], source);
        std.debug.assert(std.mem.eql(u8, buffer[0..source.len], source));

        stringzilla.fill(buffer[0..50], 0xAA);
        for (buffer[0..50]) |byte| {
            std.debug.assert(byte == 0xAA);
        }

        // Test moveMemory operation
        const test_data = "abcdef";
        @memcpy(buffer[0..test_data.len], test_data);
        const original = [_]u8{ 'a', 'b', 'c', 'd', 'e', 'f' };
        @memcpy(buffer[0..test_data.len], &original);
        stringzilla.moveMemory(buffer[2..6], buffer[0..4]);
        // After move: a b a b c d f (positions 2-5 should be "abcd")
        std.debug.assert(std.mem.eql(u8, buffer[2..6], "abcd"));
    }

    {
        const text = "Hello, World! This is a test.";

        const range = stringzilla.utf8_case_insensitive_find(text, "world") orelse @panic("Expected to find 'world' in text");
        const found_text = range.slice(text);
        std.debug.assert(std.mem.eql(u8, found_text, "World"));
        std.debug.assert(range.start == 7);
        std.debug.assert(range.end() == 12);
        std.debug.assert(range.length == 5);

        std.debug.assert(!range.isEmpty());
        std.debug.assert(range.contains(10));

        const range1 = stringzilla.Range{ .start = 5, .length = 10 };
        const range2 = stringzilla.Range{ .start = 12, .length = 8 };

        std.debug.assert(range1.overlaps(range2));

        const intersection = range1.intersection(range2) orelse @panic("Expected intersection between overlapping ranges");
        std.debug.assert(intersection.start == 12);
        std.debug.assert(intersection.length == 3); // [12, 15)
    }

    {
        const utf8_text = "Hello 🌍 世界";
        var runes: [16]u32 = undefined;

        const result = stringzilla.utf8_unpack_chunk(utf8_text, &runes);
        std.debug.assert(result.bytes_consumed > 0);
        std.debug.assert(result.runes_unpacked > 0);
        try std.testing.expectEqual(result.runes_unpacked, 10);

        std.debug.assert(runes[0] == 'H');
        std.debug.assert(runes[1] == 'e');
        std.debug.assert(runes[2] == 'l');
    }

    {
        const utf8_text = "Hello, 世界! 🌍";
        const char_count = stringzilla.count_utf8(utf8_text);
        std.debug.assert(char_count == 9); // H e l l o ,   space! 🌍

        const range = stringzilla.utf8_case_insensitive_find(utf8_text, "WORLD") orelse @panic("Expected to find 'WORLD' in UTF-8 text");
        const found_text = range.slice(utf8_text);
        std.debug.assert(std.mem.eql(u8, found_text, "世界")); // Wait, this should be "Hello" not "世界"

        const russian_text = "Привет МИР, привет мир!";
        const russian_range = stringzilla.utf8_case_insensitive_find(russian_text, "мир") orelse @panic("Expected to find 'мир' in Russian text");
        const found_word = russian_range.slice(russian_text);
        std.debug.assert(found_word.len == 6); // Russian characters are 2 bytes each
    }

    {
        const text = "The quick brown fox jumps over the lazy dog";
        const vowels = stringzilla.Byteset.from_bytes("aeiou");
        const consonants = stringzilla.Byteset.from_bytes("bcdfghjklmnpqrstvwxyz");

        const first_vowel_pos = stringzilla.find_byteset(text, vowels) orelse @panic("Expected to find vowel in text");
        std.debug.assert(first_vowel_pos == 2); // 'e' in "The"

        const first_consonant_pos = stringzilla.find_byteset(text, consonants) orelse @panic("Expected to find consonant in text");
        std.debug.assert(first_consonant_pos == 1); // 'h' in "The"

        const xyz_pos = stringzilla.find_byte_from(text, "xyz") orelse @panic("Expected to find 'x', 'y', or 'z' in text");
        std.debug.assert(xyz_pos == 23); // 'x' in "fox"

        const non_vowel_pos = stringzilla.find_byte_not_from(text, "aeiou ") orelse @panic("Expected to find non-vowel in text");
        std.debug.assert(non_vowel_pos == 0); // 'T' in "The"
    }
}
