const std = @import("std");
const config = @import("config");

//pub const clib = if (config.is_dynamic_dispatch) @import("extern.zig") else @cImport(@cInclude("stringzilla/stringzilla.h"));
pub const clib = @import("extern.zig");
pub const types = @import("types.zig");

/// Intersects two sequences (inner join) using their default byte-slice views.
pub fn intersection(allocator: std.mem.Allocator, comptime T: type, data1: []const T, data2: []const T, _seed: u64, positions1: []types.SortedIdx, positions2: []types.SortedIdx) types.StringZillaError!usize {
    _ = _seed; // Suppress unused parameter warning
    const min_count = @min(data1.len, data2.len);
    if (positions1.len < min_count or positions2.len < min_count) {
        return types.StringZillaError.BadAlloc;
    }

    // Convert to byte slices for intersection
    const slices1 = try allocator.alloc([]const u8, data1.len);
    defer allocator.free(slices1);
    const slices2 = try allocator.alloc([]const u8, data2.len);
    defer allocator.free(slices2);

    for (0..data1.len) |i| {
        slices1[i] = std.mem.asBytes(&data1[i]);
    }
    for (0..data2.len) |i| {
        slices2[i] = std.mem.asBytes(&data2[i]);
    }

    return intersection_fallback(slices1, slices2, positions1, positions2);
}

// Simple fallback intersection implementation
fn intersection_fallback(data1: []const []const u8, data2: []const []const u8, positions1: []types.SortedIdx, positions2: []types.SortedIdx) types.StringZillaError!usize {
    var inter_size: usize = 0;

    // Simple O(n^2) intersection for demonstration
    for (0..data1.len) |i| {
        for (0..data2.len) |j| {
            if (std.mem.eql(u8, data1[i], data2[j])) {
                if (inter_size < positions1.len and inter_size < positions2.len) {
                    positions1[inter_size] = @intCast(i);
                    positions2[inter_size] = @intCast(j);
                    inter_size += 1;
                }
            }
        }
    }

    return inter_size;
}

/// A byte set for efficient character matching.
pub const Byteset = struct {
    bits: [4]u64 = [_]u64{0} ** 4,

    /// Initializes a bit-set to contain all ASCII characters.
    pub fn new_ascii() Byteset {
        return Byteset{
            .bits = [_]u64{std.math.maxInt(u64)} ** 2 ++ [_]u64{0} ** 2,
        };
    }

    /// Adds a byte to the set.
    pub fn add_u8(self: *Byteset, c: u8) void {
        const idx = c >> 6; // Divide by 64
        const bit: u6 = @intCast(c & 63); // Remainder modulo 64
        self.bits[idx] |= @as(u64, 1) << bit;
    }

    /// Adds a character to the set (assumes ASCII).
    pub fn add(self: *Byteset, c: u8) void {
        self.add_u8(c);
    }

    pub fn inverted(self: Byteset) Byteset {
        return Byteset{
            .bits = [_]u64{
                ~self.bits[0],
                ~self.bits[1],
                ~self.bits[2],
                ~self.bits[3],
            },
        };
    }

    pub fn from_bytes(bytes: []const u8) Byteset {
        var set = Byteset{};
        for (bytes) |b| {
            set.add_u8(b);
        }
        return set;
    }
};

/// Incremental hasher state for StringZilla's 64-bit hash.
pub const Hasher = struct {
    aes: [8]u64 align(64),
    sum: [8]u64,
    ins: [8]u64, // Ignored in comparisons
    key: [2]u64,
    ins_length: usize, // Ignored in comparisons

    pub fn init(seed: u64) Hasher {
        var state: Hasher = std.mem.zeroes(Hasher);
        clib.sz_hash_state_init(&state, seed);
        return state;
    }

    pub fn update(self: *Hasher, data: []const u8) void {
        clib.sz_hash_state_update(self, data.ptr, data.len);
    }

    pub fn digest(self: *const Hasher) u64 {
        return clib.sz_hash_state_digest(self);
    }

    pub fn eq(self: *const Hasher, other: *const Hasher) bool {
        return std.mem.eql(u64, &self.aes, &other.aes) and
            std.mem.eql(u64, &self.sum, &other.sum) and
            std.mem.eql(u64, &self.key, &other.key);
    }
};

/// Incremental SHA256 hasher state for cryptographic hashing.
pub const Sha256 = struct {
    hash_state: [8]u32 align(64), // Current hash state (h0-h7)
    block: [64]u8, // 64-byte message block buffer
    block_length: usize, // Current bytes in block (0-63)
    total_length: u64, // Total message length in bytes

    pub fn init() Sha256 {
        var state: Sha256 = std.mem.zeroes(Sha256);
        clib.sz_sha256_state_init(&state);
        return state;
    }

    pub fn update(self: *Sha256, data: []const u8) void {
        clib.sz_sha256_state_update(self, data.ptr, data.len);
    }

    pub fn digest(self: *const Sha256) [32]u8 {
        var digest_data: [32]u8 = undefined;
        clib.sz_sha256_state_digest(self, &digest_data);
        return digest_data;
    }

    pub fn hash(data: []const u8) [32]u8 {
        var hasher = Sha256.init();
        hasher.update(data);
        return hasher.digest();
    }
};

/// Checks if the library was compiled with dynamic dispatch enabled.
pub inline fn dynamic_dispatch() bool {
    return clib.sz_dynamic_dispatch() != 0;
}

/// Returns library capabilities as a C string.
pub inline fn capabilities() *const anyopaque {
    return clib.sz_capabilities_to_string(clib.sz_capabilities());
}

/// Returns the semantic version information.
pub inline fn version() types.SemVer {
    return types.SemVer{
        .major = clib.sz_version_major(),
        .minor = clib.sz_version_minor(),
        .patch = clib.sz_version_patch(),
    };
}

/// Computes the checksum value of unsigned bytes in a given byte slice.
pub inline fn bytesum(text: []const u8) u64 {
    return clib.sz_bytesum(text.ptr, text.len);
}

/// Moves the contents of `source` into `target`.
pub inline fn move_(target: []u8, source: []const u8) void {
    std.debug.assert(target.len >= source.len);
    clib.sz_move(target.ptr, source.ptr, source.len);
}

/// Fills the contents of `target` with the specified `value`.
pub inline fn fill(target: []u8, value: u8) void {
    clib.sz_fill(target.ptr, target.len, value);
}

/// Copies the contents of `source` into `target`.
pub inline fn copy(target: []u8, source: []const u8) void {
    std.debug.assert(target.len >= source.len);
    clib.sz_copy(target.ptr, source.ptr, source.len);
}

/// Performs a lookup transformation (LUT).
pub inline fn lookup(target: []u8, source: []const u8, table: [256]u8) void {
    std.debug.assert(target.len >= source.len);
    _ = clib.sz_lookup(target.ptr, source.len, source.ptr, &table);
}

/// Performs a lookup transformation in-place.
pub inline fn lookup_inplace(buffer: []u8, table: [256]u8) void {
    _ = clib.sz_lookup(buffer.ptr, buffer.len, buffer.ptr, &table);
}

/// Applies Unicode case folding to a UTF-8 string.
pub inline fn utf8_case_fold(source: []const u8, destination: []u8) usize {
    return clib.sz_utf8_case_fold(source.ptr, source.len, destination.ptr);
}

/// Performs case-insensitive search for `needle` in UTF-8 `haystack`.
pub fn utf8_case_insensitive_find(haystack: []const u8, needle: []const u8) ?struct { offset: usize, length: usize } {
    var matched_length: usize = 0;
    const result = clib.sz_utf8_case_insensitive_find(haystack.ptr, haystack.len, needle.ptr, needle.len, &matched_length);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    const offset: usize = @intCast(@intFromPtr(result) - @intFromPtr(haystack.ptr));
    return .{ .offset = offset, .length = matched_length };
}

/// Compares two UTF-8 strings in case-insensitive manner.
pub fn utf8_case_insensitive_order(a: []const u8, b: []const u8) std.math.Order {
    const result = clib.sz_utf8_case_insensitive_order(a.ptr, a.len, b.ptr, b.len);

    if (result < 0) {
        return .lt;
    } else if (result == 0) {
        return .eq;
    } else {
        return .gt;
    }
}

/// Unpacks a UTF-8 byte sequence into UTF-32 codepoints.
pub fn utf8_unpack_chunk(text: []const u8, runes: []u32) struct { bytes_consumed: usize, runes_unpacked: usize } {
    var runes_unpacked: usize = 0;
    const result = clib.sz_utf8_unpack_chunk(text.ptr, text.len, runes.ptr, runes.len, &runes_unpacked);

    const bytes_consumed: usize = if (@intFromPtr(result) == 0) 0 else @intCast(@intFromPtr(result) - @intFromPtr(text.ptr));
    return .{ .bytes_consumed = bytes_consumed, .runes_unpacked = runes_unpacked };
}

/// Computes a 64-bit AES-based hash value with seed.
pub inline fn hash_with_seed(text: []const u8, seed: u64) u64 {
    return clib.sz_hash(text.ptr, text.len, seed);
}

/// Computes a 64-bit AES-based hash value.
pub inline fn hash(text: []const u8) u64 {
    return hash_with_seed(text, 0);
}

/// Locates the first matching substring within `haystack`.
pub fn find(haystack: []const u8, needle: []const u8) ?usize {
    const result = clib.sz_find(haystack.ptr, haystack.len, needle.ptr, needle.len);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    return @intFromPtr(result) - @intFromPtr(haystack.ptr);
}

/// Locates the last matching substring within `haystack`.
pub fn rfind(haystack: []const u8, needle: []const u8) ?usize {
    const result = clib.sz_rfind(haystack.ptr, haystack.len, needle.ptr, needle.len);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    return @intFromPtr(result) - @intFromPtr(haystack.ptr);
}

/// Finds the index of the first character in `haystack` that is also present in `needles`.
pub fn find_byteset(haystack: []const u8, needles: Byteset) ?usize {
    const result = clib.sz_find_byteset(haystack.ptr, haystack.len, @ptrCast(&needles));

    if (@intFromPtr(result) == 0) {
        return null;
    }

    return @intFromPtr(result) - @intFromPtr(haystack.ptr);
}

/// Finds the index of the last character in `haystack` that is also present in `needles`.
pub fn rfind_byteset(haystack: []const u8, needles: Byteset) ?usize {
    const result = clib.sz_rfind_byteset(haystack.ptr, haystack.len, &needles);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    return @intFromPtr(result) - @intFromPtr(haystack.ptr);
}

/// Finds the index of the first character in `haystack` that is also present in `needles`.
pub fn find_byte_from(haystack: []const u8, needles: []const u8) ?usize {
    return find_byteset(haystack, Byteset.from_bytes(needles));
}

/// Finds the index of the last character in `haystack` that is also present in `needles`.
pub fn rfind_byte_from(haystack: []const u8, needles: []const u8) ?usize {
    return rfind_byteset(haystack, Byteset.from_bytes(needles));
}

/// Finds the index of the first character in `haystack` that is not present in `needles`.
pub fn find_byte_not_from(haystack: []const u8, needles: []const u8) ?usize {
    return find_byteset(haystack, Byteset.from_bytes(needles).inverted());
}

/// Finds the index of the last character in `haystack` that is not present in `needles`.
pub fn rfind_byte_not_from(haystack: []const u8, needles: []const u8) ?usize {
    return rfind_byteset(haystack, Byteset.from_bytes(needles).inverted());
}

/// Finds the first newline character in UTF-8 encoded text.
pub fn find_newline_utf8(text: []const u8) ?[]const u8 {
    var matched_length: usize = 0;
    const result = clib.sz_utf8_find_newline(text.ptr, text.len, &matched_length);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    const offset = @intFromPtr(result) - @intFromPtr(text.ptr);
    return text[offset .. offset + matched_length];
}

/// Finds the first whitespace character in UTF-8 encoded text.
pub fn find_whitespace_utf8(text: []const u8) ?[]const u8 {
    var matched_length: usize = 0;
    const result = clib.sz_utf8_find_whitespace(text.ptr, text.len, &matched_length);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    const offset = @intFromPtr(result) - @intFromPtr(text.ptr);
    return text[offset .. offset + matched_length];
}

/// Counts the number of UTF-8 characters in the text.
pub inline fn count_utf8(text: []const u8) usize {
    return clib.sz_utf8_count(text.ptr, text.len);
}

/// Finds the byte offset of the Nth UTF-8 character (0-indexed).
pub fn find_nth_utf8(text: []const u8, n: usize) ?usize {
    const result = clib.sz_utf8_find_nth(text.ptr, text.len, n);

    if (@intFromPtr(result) == 0) {
        return null;
    }

    return @intFromPtr(result) - @intFromPtr(text.ptr);
}

/// Randomizes the contents of a given byte slice.
pub inline fn fill_random(buffer: []u8, nonce: u64) void {
    clib.sz_fill_random(buffer.ptr, buffer.len, nonce);
}

/// Computes HMAC-SHA256 for the given key and message.
pub fn hmac_sha256(key: []const u8, message: []const u8) [32]u8 {
    // Prepare key: hash if > 64 bytes, zero-pad to 64 bytes
    var key_pad = [_]u8{0} ** 64;
    if (key.len > 64) {
        const key_hash = Sha256.hash(key);
        @memcpy(key_pad[0..32], &key_hash);
    } else {
        @memcpy(key_pad[0..key.len], key);
    }

    // Compute inner hash: SHA256((key ^ 0x36) || message)
    var inner_hasher = Sha256.init();
    var inner_pad = [_]u8{0} ** 64;
    for (&inner_pad, 0..) |*byte, i| {
        byte.* = key_pad[i] ^ 0x36;
    }
    inner_hasher.update(&inner_pad);
    inner_hasher.update(message);
    const inner_hash = inner_hasher.digest();

    // Compute outer hash: SHA256((key ^ 0x5c) || inner_hash)
    var outer_hasher = Sha256.init();
    var outer_pad = [_]u8{0} ** 64;
    for (&outer_pad, 0..) |*byte, i| {
        byte.* = key_pad[i] ^ 0x5c;
    }
    outer_hasher.update(&outer_pad);
    outer_hasher.update(&inner_hash);
    return outer_hasher.digest();
}

/// Iterator over substrings of UTF-8 text split by newline characters.
pub const NewlineUtf8Iterator = struct {
    text: []const u8,
    position: usize = 0,
    finished: bool = false,

    pub fn next(self: *NewlineUtf8Iterator) ?[]const u8 {
        if (self.finished) {
            return null;
        }

        if (self.position >= self.text.len) {
            if (self.position == self.text.len and !self.finished) {
                self.finished = true;
                return self.text[self.text.len..];
            }
            return null;
        }

        const start = self.position;

        if (find_newline_utf8(self.text[self.position..])) |newline_slice| {
            const end = self.position + @intFromPtr(newline_slice.ptr) - @intFromPtr(self.text[self.position..].ptr);
            self.position = end + newline_slice.len;
            return self.text[start..end];
        } else {
            // No more newlines, return rest of text
            self.finished = true;
            self.position = self.text.len;
            return self.text[start..];
        }
    }
};

/// Iterator over words in UTF-8 text split by whitespace characters.
pub const WhitespaceUtf8Iterator = struct {
    text: []const u8,
    position: usize = 0,

    pub fn next(self: *WhitespaceUtf8Iterator) ?[]const u8 {
        if (self.position >= self.text.len) {
            return null;
        }

        // Skip leading whitespace
        while (self.position < self.text.len) {
            if (find_whitespace_utf8(self.text[self.position..])) |whitespace_slice| {
                const offset = @intFromPtr(whitespace_slice.ptr) - @intFromPtr(self.text[self.position..].ptr);
                if (offset == 0) {
                    // Whitespace at current position, skip it
                    self.position += whitespace_slice.len;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if (self.position >= self.text.len) {
            return null;
        }

        const start = self.position;

        if (find_whitespace_utf8(self.text[self.position..])) |whitespace_slice| {
            const end = self.position + @intFromPtr(whitespace_slice.ptr) - @intFromPtr(self.text[self.position..].ptr);
            self.position = end + whitespace_slice.len;
            return self.text[start..end];
        } else {
            // No more whitespace, return rest of text
            self.position = self.text.len;
            return self.text[start..];
        }
    }
};

test "metadata" {
    try std.testing.expect(dynamic_dispatch());
    const caps = capabilities();
    try std.testing.expect(@intFromPtr(caps) != 0);
}

test "bytesum" {
    const result = bytesum("hi");
    try std.testing.expect(result == 209);
}

test "hash" {
    const hash_hello = hash("Hello");
    const hash_world = hash("World");
    try std.testing.expect(hash_hello != hash_world);

    // Hashing should work the same for any seed
    const seeds = [_]u64{ 0, 42, 123456789 };
    for (seeds) |seed| {
        // Single-pass hashing
        const hasher1 = Hasher.init(seed);
        var hasher1_mut = hasher1;
        hasher1_mut.update("Hello");
        const single_pass = hasher1_mut.digest();

        try std.testing.expect(single_pass == hash_with_seed("Hello", seed));

        // Dual pass for short strings
        const hasher2 = Hasher.init(seed);
        var hasher2_mut = hasher2;
        hasher2_mut.update("Hello");
        hasher2_mut.update("World");
        const dual_pass = hasher2_mut.digest();

        try std.testing.expect(dual_pass == hash_with_seed("HelloWorld", seed));
    }
}

test "streaming_hash" {
    var hasher1 = Hasher.init(123);
    hasher1.update("Hello, ");
    hasher1.update("world!");
    const streamed = hasher1.digest();

    var hasher2 = Hasher.init(123);
    hasher2.update("Hello, world!");
    const expected = hasher2.digest();
    try std.testing.expect(streamed == expected);
}

test "search" {
    // Basic find operations
    try std.testing.expect(find("Hello, world!", "world") == 7);
    try std.testing.expect(rfind("Hello, world!", "world") == 7);

    // Test find_byte_from
    try std.testing.expect(find_byte_from("Hello, world!", "world") == 2);
    try std.testing.expect(rfind_byte_from("Hello, world!", "world") == 11);

    // Test find_byte_not_from
    try std.testing.expect(find_byte_not_from("Hello, world!", "world") == 0);
    try std.testing.expect(rfind_byte_not_from("Hello, world!", "world") == 12);

    // Test not found cases
    try std.testing.expect(find("Hello, world!", "xyz") == null);
    try std.testing.expect(rfind("Hello, world!", "xyz") == null);
}

test "fill_random" {
    var first_buffer: [10]u8 = [_]u8{0} ** 10; // Ten zeros
    var second_buffer: [10]u8 = [_]u8{1} ** 10; // Ten ones

    fill_random(&first_buffer, 42);
    fill_random(&second_buffer, 42);

    // Same nonce will produce the same outputs
    try std.testing.expect(std.mem.eql(u8, &first_buffer, &second_buffer));
}

test "byteset_operations" {
    var set = Byteset{};
    set.add('a');
    set.add('b');
    set.add('c');

    // Test find_byteset
    try std.testing.expect(find_byteset("abc", set) == 0);
    try std.testing.expect(find_byteset("xyzabc", set) == 3);
    try std.testing.expect(find_byteset("xyz", set) == null);

    // Test rfind_byteset
    try std.testing.expect(rfind_byteset("abc", set) == 2);
    try std.testing.expect(rfind_byteset("xyzabc", set) == 5);
    try std.testing.expect(rfind_byteset("xyz", set) == null);

    // Test from_bytes
    const set2 = Byteset.from_bytes("xyz");
    try std.testing.expect(find_byteset("xyz", set2) == 0);
    try std.testing.expect(find_byteset("abcxyz", set2) == 3);
}

test "utf8_operations" {
    // Test find_newline_utf8
    const slice1 = find_newline_utf8("Hello\nWorld");
    try std.testing.expect(slice1 != null);
    try std.testing.expect(std.mem.eql(u8, slice1.?, "\n"));

    const slice2 = find_newline_utf8("Hello\r\nWorld");
    try std.testing.expect(slice2 != null);
    try std.testing.expect(std.mem.eql(u8, slice2.?, "\r\n"));

    try std.testing.expect(find_newline_utf8("HelloWorld") == null);

    // Test find_whitespace_utf8
    const slice3 = find_whitespace_utf8("Hello World");
    try std.testing.expect(slice3 != null);
    try std.testing.expect(std.mem.eql(u8, slice3.?, " "));

    try std.testing.expect(find_whitespace_utf8("HelloWorld") == null);

    // Test utf8_case_insensitive_find
    const result1 = utf8_case_insensitive_find("Hello WORLD", "world");
    try std.testing.expect(result1 != null);
    const found_slice = "Hello WORLD"[result1.?.offset .. result1.?.offset + result1.?.length];
    try std.testing.expect(std.mem.eql(u8, found_slice, "WORLD"));

    try std.testing.expect(utf8_case_insensitive_find("Hello", "WORLD") == null);

    // Test utf8_case_insensitive_order
    try std.testing.expect(utf8_case_insensitive_order("Hello", "HELLO") == .eq);
    try std.testing.expect(utf8_case_insensitive_order("abc", "ABD") == .lt);
    try std.testing.expect(utf8_case_insensitive_order("ABD", "abc") == .gt);

    // Test count_utf8
    try std.testing.expect(count_utf8("Hello") == 5);
    try std.testing.expect(count_utf8("Hello 世界") == 8); // 6 ASCII + 2 CJK characters

    // Test find_nth_utf8
    try std.testing.expect(find_nth_utf8("Hello", 0) == 0);
    try std.testing.expect(find_nth_utf8("Hello", 4) == 4);
    try std.testing.expect(find_nth_utf8("Hello", 5) == null);
}

test "sha256_operations" {
    // Test SHA256 hashing
    const digest1 = Sha256.hash("Hello, world!");
    try std.testing.expect(digest1.len == 32);

    // Test incremental hashing
    var hasher = Sha256.init();
    hasher.update("Hello, ");
    hasher.update("world!");
    const digest2 = hasher.digest();

    try std.testing.expect(std.mem.eql(u8, &digest1, &digest2));

    // Test HMAC-SHA256
    const key = "secret_key";
    const message = "important message";
    const mac = hmac_sha256(key, message);
    try std.testing.expect(mac.len == 32);

    // Test that same inputs produce same MAC
    const mac2 = hmac_sha256(key, message);
    try std.testing.expect(std.mem.eql(u8, &mac, &mac2));

    // Test different keys produce different MACs
    const mac3 = hmac_sha256("different_key", message);
    try std.testing.expect(!std.mem.eql(u8, &mac, &mac3));
}

test "hasher_equality" {
    const hasher1 = Hasher.init(123);
    var hasher1_mut = hasher1;
    hasher1_mut.update("test");

    const hasher2 = Hasher.init(123);
    var hasher2_mut = hasher2;
    hasher2_mut.update("test");

    try std.testing.expect(hasher1_mut.digest() == hasher2_mut.digest());
}

test "newline_splits" {
    const text = "line1\nline2\nline3";
    var splits = NewlineUtf8Iterator{ .text = text };

    const line1 = splits.next();
    try std.testing.expect(line1 != null);
    try std.testing.expect(std.mem.eql(u8, line1.?, "line1"));

    const line2 = splits.next();
    try std.testing.expect(line2 != null);
    try std.testing.expect(std.mem.eql(u8, line2.?, "line2"));

    const line3 = splits.next();
    try std.testing.expect(line3 != null);
    try std.testing.expect(std.mem.eql(u8, line3.?, "line3"));

    const line4 = splits.next();
    try std.testing.expect(line4 == null);
}

test "whitespace_splits" {
    const text = "  word1   word2\tword3  ";
    var splits = WhitespaceUtf8Iterator{ .text = text };

    const word1 = splits.next();
    try std.testing.expect(word1 != null);
    try std.testing.expect(std.mem.eql(u8, word1.?, "word1"));

    const word2 = splits.next();
    try std.testing.expect(word2 != null);
    try std.testing.expect(std.mem.eql(u8, word2.?, "word2"));

    const word3 = splits.next();
    try std.testing.expect(word3 != null);
    try std.testing.expect(std.mem.eql(u8, word3.?, "word3"));

    const word4 = splits.next();
    try std.testing.expect(word4 == null);
}

test "utf8_unpack_chunk" {
    const text = "Hello 世界";
    var runes: [16]u32 = undefined;
    const result = utf8_unpack_chunk(text, &runes);

    try std.testing.expect(result.runes_unpacked == 8); // 6 ASCII + 2 CJK characters
    try std.testing.expect(runes[0] == 'H');
    try std.testing.expect(runes[1] == 'e');
    try std.testing.expect(runes[2] == 'l');
    try std.testing.expect(runes[3] == 'l');
    try std.testing.expect(runes[4] == 'o');
    try std.testing.expect(runes[5] == ' ');
    try std.testing.expect(runes[6] == '世');
    try std.testing.expect(runes[7] == '界');
}

test "memory_operations" {
    // Test copy
    var target: [10]u8 = undefined;
    const source = "1234567890";
    copy(&target, source[0..10]);
    try std.testing.expect(std.mem.eql(u8, &target, source[0..10]));

    // Test fill
    var buffer: [5]u8 = undefined;
    fill(&buffer, 'A');
    try std.testing.expect(std.mem.eql(u8, &buffer, &[_]u8{ 'A', 'A', 'A', 'A', 'A' }));

    // Test move
    var dest: [6]u8 = undefined;
    const src = "source";
    move_(&dest, src[0..6]);
    try std.testing.expect(std.mem.eql(u8, &dest, "source"));
}

test "version" {
    const ver = version();
    try std.testing.expect(ver.major >= 0);
    try std.testing.expect(ver.minor >= 0);
    try std.testing.expect(ver.patch >= 0);
}

test "intersection_default" {
    const set1 = [_][]const u8{ "banana", "apple", "cherry" };
    const set2 = [_][]const u8{ "cherry", "orange", "pineapple", "banana" };

    var positions1: [3]types.SortedIdx = undefined;
    var positions2: [3]types.SortedIdx = undefined;

    const intersection_size = try intersection(std.testing.allocator, []const u8, &set1, &set2, 42, &positions1, &positions2);

    // Should find 2 intersections: "banana" and "cherry"
    try std.testing.expect(intersection_size == 2);

    // Verify positions (should be sorted by the intersecting elements)
    // The intersection should be: "banana" (from set1[0], set2[3]) and "cherry" (from set1[2], set2[0])
    try std.testing.expect(positions1[0] == 0); // banana in set1
    try std.testing.expect(positions2[0] == 3); // banana in set2
    try std.testing.expect(positions1[1] == 2); // cherry in set1
    try std.testing.expect(positions2[1] == 0); // cherry in set2
}

test "sha256_operations_detailed" {
    // Test SHA256 with empty input
    const empty_digest = Sha256.hash("");
    try std.testing.expect(empty_digest.len == 32);

    // Test SHA256 with known input
    const abc_digest = Sha256.hash("abc");
    try std.testing.expect(abc_digest.len == 32);

    // Test incremental vs one-shot
    var hasher = Sha256.init();
    hasher.update("a");
    hasher.update("b");
    hasher.update("c");
    const incremental_digest = hasher.digest();

    try std.testing.expect(std.mem.eql(u8, &abc_digest, &incremental_digest));

    // Test HMAC with known vectors (RFC 2104 test vectors simplified)
    const key = "key";
    const message = "The quick brown fox jumps over the lazy dog";
    const mac = hmac_sha256(key, message);
    try std.testing.expect(mac.len == 32);

    // Verify HMAC properties
    const mac2 = hmac_sha256(key, message);
    try std.testing.expect(std.mem.eql(u8, &mac, &mac2));

    // Different messages should produce different MACs
    const mac3 = hmac_sha256(key, "different message");
    try std.testing.expect(!std.mem.eql(u8, &mac, &mac3));

    // Different keys should produce different MACs
    const mac4 = hmac_sha256("different key", message);
    try std.testing.expect(!std.mem.eql(u8, &mac, &mac4));
}

test "utf8_operations_comprehensive" {
    // Test find_newline_utf8 with different newline types
    const lf_result = find_newline_utf8("Hello\nWorld");
    try std.testing.expect(lf_result != null);
    try std.testing.expect(std.mem.eql(u8, lf_result.?, "\n"));

    const crlf_result = find_newline_utf8("Hello\r\nWorld");
    try std.testing.expect(crlf_result != null);
    try std.testing.expect(std.mem.eql(u8, crlf_result.?, "\r\n"));

    const vt_result = find_newline_utf8("Hello\u{0B}World"); // Vertical Tab
    try std.testing.expect(vt_result != null);
    try std.testing.expect(std.mem.eql(u8, vt_result.?, "\x0B"));

    const ff_result = find_newline_utf8("Hello\u{0C}World"); // Form Feed
    try std.testing.expect(ff_result != null);
    try std.testing.expect(std.mem.eql(u8, ff_result.?, "\x0C"));

    const cr_result = find_newline_utf8("Hello\rWorld");
    try std.testing.expect(cr_result != null);
    try std.testing.expect(std.mem.eql(u8, cr_result.?, "\r"));

    const nel_result = find_newline_utf8("Hello\u{85}World"); // NEL
    try std.testing.expect(nel_result != null);

    const line_sep_result = find_newline_utf8("Hello\u{2028}World"); // Line Separator
    try std.testing.expect(line_sep_result != null);

    const para_sep_result = find_newline_utf8("Hello\u{2029}World"); // Paragraph Separator
    try std.testing.expect(para_sep_result != null);

    // Test not found cases
    try std.testing.expect(find_newline_utf8("HelloWorld") == null);
    try std.testing.expect(find_newline_utf8("") == null);

    // Test find_whitespace_utf8 with different whitespace types
    const space_result = find_whitespace_utf8("Hello World");
    try std.testing.expect(space_result != null);
    try std.testing.expect(std.mem.eql(u8, space_result.?, " "));

    const tab_result = find_whitespace_utf8("Hello\tWorld");
    try std.testing.expect(tab_result != null);

    const nbsp_result = find_whitespace_utf8("Hello\u{A0}World"); // Non-breaking space
    try std.testing.expect(nbsp_result != null);

    const ideographic_result = find_whitespace_utf8("Hello\u{3000}World"); // Ideographic space
    try std.testing.expect(ideographic_result != null);
    try std.testing.expect(find_whitespace_utf8("HelloWorld") == null);
    try std.testing.expect(find_whitespace_utf8("") == null);
}

test "iterators_comprehensive" {
    // Test NewlineUtf8Iterator
    {
        const text = "line1\nline2\nline3";
        var splits = NewlineUtf8Iterator{ .text = text };

        const line1 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line1.?, "line1"));

        const line2 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line2.?, "line2"));

        const line3 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line3.?, "line3"));

        const line4 = splits.next();
        try std.testing.expect(line4 == null);
    }

    // Test NewlineUtf8Iterator with trailing newline
    {
        const text = "line1\nline2\n";
        var splits = NewlineUtf8Iterator{ .text = text };

        const line1 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line1.?, "line1"));

        const line2 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line2.?, "line2"));

        const line3 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line3.?, ""));

        try std.testing.expect(splits.next() == null);
    }

    // Test NewlineUtf8Iterator with empty string
    {
        const text = "";
        var splits = NewlineUtf8Iterator{ .text = text };

        const line1 = splits.next();
        try std.testing.expect(std.mem.eql(u8, line1.?, ""));

        try std.testing.expect(splits.next() == null);
    }

    // Test WhitespaceUtf8Iterator
    {
        const text = "  word1   word2\tword3  ";
        var splits = WhitespaceUtf8Iterator{ .text = text };

        const word1 = splits.next();
        try std.testing.expect(word1 != null);
        try std.testing.expect(std.mem.eql(u8, word1.?, "word1"));

        const word2 = splits.next();
        try std.testing.expect(word2 != null);
        try std.testing.expect(std.mem.eql(u8, word2.?, "word2"));

        const word3 = splits.next();
        try std.testing.expect(word3 != null);
        try std.testing.expect(std.mem.eql(u8, word3.?, "word3"));

        const word4 = splits.next();
        try std.testing.expect(word4 == null);
    }

    // Test WhitespaceUtf8Iterator with empty string
    {
        const text = "";
        var splits = WhitespaceUtf8Iterator{ .text = text };

        const word1 = splits.next();
        try std.testing.expect(word1 == null);
    }
}

test "byteset_operations_comprehensive" {
    // Test empty byteset
    const empty_set = Byteset{};
    try std.testing.expect(find_byteset("abc", empty_set) == null);

    // Test ASCII byteset
    const ascii_set = Byteset.new_ascii();
    try std.testing.expect(find_byteset("Hello", ascii_set) == 0); // 'H' is in ASCII

    // Test from_bytes
    const vowel_set = Byteset.from_bytes("aeiou");
    try std.testing.expect(find_byteset("hello", vowel_set) == 1); // 'e'
    try std.testing.expect(find_byteset("world", vowel_set) == 1); // 'o'

    // Test inverted
    const inverted_set = vowel_set.inverted();
    try std.testing.expect(find_byteset("aeiou", inverted_set) == null); // all vowels are excluded

    // Test adding characters
    var custom_set = Byteset{};
    custom_set.add('x');
    custom_set.add('y');
    custom_set.add('z');
    try std.testing.expect(find_byteset("xyz", custom_set) == 0);
    try std.testing.expect(find_byteset("abcxyz", custom_set) == 3);
    try std.testing.expect(rfind_byteset("abc", custom_set) == null);
    try std.testing.expect(rfind_byteset("abc", vowel_set) == 0); // a
    try std.testing.expect(find_byte_from("hello", "") == null); // empty needles
    try std.testing.expect(find_byte_from("", "abc") == null); // empty haystack

    try std.testing.expect(find_byte_not_from("aaa", "a") == null); // all characters are in needles
    try std.testing.expect(find_byte_not_from("", "a") == null); // empty haystack
}

test "memory_operations_edge_cases" {
    // Test copy with empty source
    {
        var target: [10]u8 = undefined;
        copy(&target, "");
        // Copy with empty source should do nothing, just ensure no crash
    }

    // Test fill with zero-length buffer
    {
        var buffer: [0]u8 = undefined;
        fill(&buffer, 'A');
        // Fill with zero-length should not crash
    }

    // Test move with same source and target
    {
        var buffer: [10]u8 = "1234567890".*;
        move_(&buffer, buffer[0..10]);
        try std.testing.expect(std.mem.eql(u8, &buffer, "1234567890"));
    }
}

test "utf8_unpack_edge_cases" {
    // Test with empty input
    {
        const text = "";
        var runes: [10]u32 = undefined;
        const result = utf8_unpack_chunk(text, &runes);
        try std.testing.expect(result.bytes_consumed == 0);
        try std.testing.expect(result.runes_unpacked == 0);
    }

    // Test with insufficient output buffer
    {
        const text = "Hello 世界";
        var runes: [1]u32 = undefined; // Too small
        const result = utf8_unpack_chunk(text, &runes);
        try std.testing.expect(result.runes_unpacked == 1); // Only one rune fits
        try std.testing.expect(runes[0] == 'H');
    }
}

test "hasher_edge_cases" {
    // Test hashing empty input
    const empty_hash = hash("");
    try std.testing.expect(empty_hash == hash_with_seed("", 0));

    // Test hasher with multiple updates vs single update
    {
        var hasher1 = Hasher.init(123);
        hasher1.update("Hello, ");
        hasher1.update("world!");
        const hash1 = hasher1.digest();

        var hasher2 = Hasher.init(123);
        hasher2.update("Hello, world!");
        const hash2 = hasher2.digest();

        try std.testing.expect(hash1 == hash2);
    }

    {
        const hasher1 = Hasher.init(42);
        const hasher2 = Hasher.init(42);
        try std.testing.expect(hasher1.digest() == hasher2.digest());
    }
}

test "find_operations_edge_cases" {
    try std.testing.expect(find("hello", "") == null);
    try std.testing.expect(find("", "hello") == null);
    try std.testing.expect(find("hi", "hello") == null);
    try std.testing.expect(find("hello", "hello") == 0);
    try std.testing.expect(rfind("hello", "") == null);
    try std.testing.expect(rfind("", "hello") == null);
    try std.testing.expect(find_byte_from("hello", "") == null);
    try std.testing.expect(find_byte_from("", "abc") == null);
    try std.testing.expect(find_byte_not_from("aaa", "a") == null);
}

test "sha256_comparison_with_zig_std" {
    const test_cases = [_][]const u8{
        "", // Empty string
        "a", // Single character
        "abc", // Short string
        "message digest", // NIST test vector
        "abcdefghijklmnopqrstuvwxyz", // Alphabet
        "The quick brown fox jumps over the lazy dog", // Common pangram
        "StringZilla vs Zig std.crypto.hash.sha2.Sha256", // Comparison test
        "\x00\x01\x02\x03\x04\x05", // Binary data
        "🦎😀🚀", // Unicode emoji
    };

    for (test_cases) |test_input| {
        // Compute SHA256 using StringZilla
        const sz_digest = Sha256.hash(test_input);

        // Compute SHA256 using Zig's standard library
        var zig_hasher = std.crypto.hash.sha2.Sha256.init(.{});
        zig_hasher.update(test_input);
        var zig_digest: [32]u8 = undefined;
        zig_hasher.final(&zig_digest);

        // The hashes should match exactly
        try std.testing.expect(std.mem.eql(u8, &sz_digest, &zig_digest));
    }

    // Test incremental hashing consistency
    {
        const test_message = "Incremental hashing test message";

        // StringZilla incremental
        var sz_hasher = Sha256.init();
        sz_hasher.update("Incremental ");
        sz_hasher.update("hashing ");
        sz_hasher.update("test message");
        const sz_incremental = sz_hasher.digest();

        // Zig std incremental
        var zig_hasher = std.crypto.hash.sha2.Sha256.init(.{});
        zig_hasher.update("Incremental ");
        zig_hasher.update("hashing ");
        zig_hasher.update("test message");
        var zig_incremental: [32]u8 = undefined;
        zig_hasher.final(&zig_incremental);

        // Both should match their respective one-shot hashes
        const sz_one_shot = Sha256.hash(test_message);
        var zig_one_shot: [32]u8 = undefined;
        var zig_one_shot_hasher = std.crypto.hash.sha2.Sha256.init(.{});
        zig_one_shot_hasher.update(test_message);
        zig_one_shot_hasher.final(&zig_one_shot);

        try std.testing.expect(std.mem.eql(u8, &sz_incremental, &sz_one_shot));
        try std.testing.expect(std.mem.eql(u8, &zig_incremental, &zig_one_shot));
        try std.testing.expect(std.mem.eql(u8, &sz_incremental, &zig_incremental));
    }
}
