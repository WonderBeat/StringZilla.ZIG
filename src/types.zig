pub const SortedIdx = usize;

/// C library sequence type
pub const sz_sequence_t = extern struct {
    handle: *const anyopaque,
    count: usize,
    get_start: *const fn (handle: *const anyopaque, idx: usize) *const anyopaque,
    get_length: *const fn (handle: *const anyopaque, idx: usize) usize,
};

pub const SemVer = struct {
    major: i32,
    minor: i32,
    patch: i32,
};

pub const Status = enum(i32) {
    Success = 0,
    BadAlloc = -10,
    InvalidUtf8 = -12,
    ContainsDuplicates = -13,
    OverflowRisk = -14,
    UnexpectedDimensions = -15,
    MissingGpu = -16,
    DeviceCodeMismatch = -17,
    DeviceMemoryMismatch = -18,
    StatusUnknown = -1,
};

pub const StringZillaError = error{
    BadAlloc,
    OutOfMemory,
    InvalidUtf8,
    ContainsDuplicates,
    OverflowRisk,
    UnexpectedDimensions,
    MissingGpu,
    DeviceCodeMismatch,
    DeviceMemoryMismatch,
    StatusUnknown,
};

fn statusToResult(status: Status) StringZillaError!void {
    return switch (status) {
        .Success => {},
        .BadAlloc => StringZillaError.BadAlloc,
        .InvalidUtf8 => StringZillaError.InvalidUtf8,
        .ContainsDuplicates => StringZillaError.ContainsDuplicates,
        .OverflowRisk => StringZillaError.OverflowRisk,
        .UnexpectedDimensions => StringZillaError.UnexpectedDimensions,
        .MissingGpu => StringZillaError.MissingGpu,
        .DeviceCodeMismatch => StringZillaError.DeviceCodeMismatch,
        .DeviceMemoryMismatch => StringZillaError.DeviceMemoryMismatch,
        .StatusUnknown => StringZillaError.StatusUnknown,
    };
}
