const types = @import("types.zig");

// ----------------------------------------------------------------------
// C Library function declarations
// ----------------------------------------------------------------------

// Version and capabilities
pub extern fn sz_dynamic_dispatch() i32;
pub extern fn sz_version_major() i32;
pub extern fn sz_version_minor() i32;
pub extern fn sz_version_patch() i32;
pub extern fn sz_capabilities() u32;
pub extern fn sz_capabilities_to_string(caps: u32) *const anyopaque;
// Memory operations
pub extern fn sz_copy(target: *const anyopaque, source: *const anyopaque, length: usize) void;
pub extern fn sz_fill(target: *const anyopaque, length: usize, value: u8) void;
pub extern fn sz_move(target: *const anyopaque, source: *const anyopaque, length: usize) void;
pub extern fn sz_fill_random(text: *anyopaque, length: usize, seed: u64) void;
pub extern fn sz_lookup(target: *const anyopaque, length: usize, source: *const anyopaque, lut: [*]const u8) *const anyopaque;
// Search operations
pub extern fn sz_find(haystack: *const anyopaque, haystack_length: usize, needle: *const anyopaque, needle_length: usize) *const anyopaque;
pub extern fn sz_rfind(haystack: *const anyopaque, haystack_length: usize, needle: *const anyopaque, needle_length: usize) *const anyopaque;
pub extern fn sz_find_byteset(haystack: *const anyopaque, haystack_length: usize, byteset: *const anyopaque) *const anyopaque;
pub extern fn sz_rfind_byteset(haystack: *const anyopaque, haystack_length: usize, byteset: *const anyopaque) *const anyopaque;
// UTF-8 operations
pub extern fn sz_utf8_count(text: *const anyopaque, length: usize) usize;
pub extern fn sz_utf8_find_nth(text: *const anyopaque, length: usize, n: usize) *const anyopaque;
pub extern fn sz_utf8_unpack_chunk(text: *const anyopaque, length: usize, runes: [*]u32, runes_capacity: usize, runes_unpacked: *usize) *const anyopaque;
pub extern fn sz_utf8_find_newline(text: *const anyopaque, length: usize, matched_length: *usize) *const anyopaque;
pub extern fn sz_utf8_find_whitespace(text: *const anyopaque, length: usize, matched_length: *usize) *const anyopaque;
pub extern fn sz_utf8_case_fold(source: *const anyopaque, source_length: usize, destination: *anyopaque) usize;
pub extern fn sz_utf8_case_insensitive_find(haystack: *const anyopaque, haystack_length: usize, needle: *const anyopaque, needle_length: usize, matched_length: *usize) *const anyopaque;
pub extern fn sz_utf8_case_insensitive_order(a: *const anyopaque, a_length: usize, b: *const anyopaque, b_length: usize) i32;
// Hash operations
pub extern fn sz_bytesum(text: *const anyopaque, length: usize) u64;
pub extern fn sz_hash(text: *const anyopaque, length: usize, seed: u64) u64;
pub extern fn sz_hash_state_init(state: *const anyopaque, seed: u64) void;
pub extern fn sz_hash_state_update(state: *const anyopaque, text: *const anyopaque, length: usize) void;
pub extern fn sz_hash_state_digest(state: *const anyopaque) u64;
pub extern fn sz_sha256_state_init(state: *const anyopaque) void;
pub extern fn sz_sha256_state_update(state: *const anyopaque, data: *const anyopaque, length: usize) void;
pub extern fn sz_sha256_state_digest(state: *const anyopaque, digest: [*]u8) void;
// Sequence operations
pub extern fn sz_sequence_from_null_terminated_strings(strings: [*]*const u8, count: usize) types.sz_sequence_t;
pub extern fn sz_sequence_from_null_terminated_bytes(strings: [*]*const u8, lengths: [*]usize, count: usize) types.sz_sequence_t;
pub extern fn sz_sequence_argsort(sequence: *const types.sz_sequence_t, alloc: *const anyopaque, order: [*]types.SortedIdx) types.Status;
pub extern fn sz_sequence_intersect(first_sequence: *const types.sz_sequence_t, second_sequence: *const types.sz_sequence_t, alloc: *const anyopaque, seed: u64, intersection_size: *usize, first_positions: [*]types.SortedIdx, second_positions: [*]types.SortedIdx) types.Status;
