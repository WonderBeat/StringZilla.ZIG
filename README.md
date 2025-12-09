# StringZilla.ZIG

A high-performance Zig wrapper for [StringZilla](https://github.com/ashvardanian/StringZilla) - a SIMD-accelerated string processing library designed for massive datasets

## Overview

StringZilla.ZIG provides idiomatic Zig bindings to the StringZilla C library

## Features

- **String Search**: `find()`, `rfind()`, `find_byteset()`, `find_byte_from()`
- **Hashing**: `bytesum()`, `hash()`, `hmac_sha256()`
- **UTF-8 Support**: Character counting and Unicode-aware operations
- **Memory Operations**: `copy()`, `move()`, `fill()`, `lookup()`
- **Bit Manipulation**: `Bitset` for efficient pattern matching

## Requirements

- Zig 0.15.0

## Installation

Add StringZilla.ZIG to your project using Zig's package manager:

```bash
zig fetch --save=stringzilla https://github.com/WonderBeat/StringZilla.ZIG/archive/refs/tags/<RELEASE-VERSION>.tar.gz
```

Then add it to your `build.zig`:

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Add StringZilla.ZIG as a dependency
    const stringzilla = b.dependency("stringzilla", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "my-app",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Import the StringZilla module
    exe.root_module.addImport("stringzilla", stringzilla.module("stringzilla"));

    b.installArtifact(exe);
}
```

## Usage

### Basic String Operations

```zig
const std = @import("std");
const stringzilla = @import("stringzilla");

pub fn main() !void {
    const haystack = "Hello, World!";
    const needle = "World";

    // Find substring
    if (stringzilla.find(haystack, needle)) |pos| {
        std.debug.print("Found '{s}' at position {}\n", .{ needle, pos });
    }

    // Reverse find
    if (stringzilla.rfind(haystack, "o")) |pos| {
        std.debug.print("Last 'o' at position {}\n", .{pos});
    }

    // Count characters (UTF-8 aware)
    const char_count = stringzilla.count_utf8(haystack);
    std.debug.print("Character count: {}\n", .{char_count});
}
```

### Hashing and Checksums

```zig
// Simple checksum
const checksum = stringzilla.bytesum("Hello, World!");
std.debug.print("Bytesum: {}\n", .{checksum});

// 64-bit hash
const hash_val = stringzilla.hash("Hello, World!");
std.debug.print("Hash: {}\n", .{hash_val});

// HMAC-SHA256 (requires key)
const hmac = stringzilla.hmac_sha256("message", "secret-key");
```

### Character Set Operations

```zig
const text = "The quick brown fox jumps over the lazy dog";

// Create a character set
const vowels = stringzilla.Byteset.from_bytes("aeiou");

// Find first character from set
if (stringzilla.find_byteset(text, vowels)) |pos| {
    std.debug.print("First vowel at position {}\n", .{pos});
}

// Alternative method
if (stringzilla.find_byte_from(text, "aeiou")) |pos| {
    std.debug.print("First vowel (using find_byte_from) at position {}\n", .{pos});
}
```

### Memory Operations

```zig
var buffer: [100]u8 = undefined;
const source = "Copy this text!";

// Copy memory
stringzilla.copy(buffer[0..source.len], source);

// Fill with pattern
stringzilla.fill(buffer[0..50], 0xAA);

// Move memory (handles overlap)
stringzilla.move_(buffer[10..], buffer[0..50]);
```

### Bit Manipulation

```zig
// Create a bitset for pattern matching
var pattern = stringzilla.Bitset.init();
pattern.set_range(65, 26); // A-Z letters

// Check if character is in set
if (pattern.contains('A')) {
    std.debug.print("A is in the pattern\n", .{});
}
```


### UTF-8 Processing

```zig
const utf8_text = "Hello, 世界! 🌍";

// Count Unicode characters
const char_count = stringzilla.count_utf8(utf8_text);
std.debug.print("Unicode characters: {}\n", .{char_count});

// Find newline characters (UTF-8 aware)
if (stringzilla.find_newline_utf8(utf8_text)) |pos| {
    std.debug.print("Newline at position {}\n", .{pos});
}

// Find whitespace (UTF-8 aware)
if (stringzilla.find_whitespace_utf8(utf8_text)) |pos| {
    std.debug.print("Whitespace at position {}\n", .{pos});
}
```

## Building

### Build the Example

```bash
zig build run
```

### Run Tests

```bash
zig build test
```

## Current Limitations

- Only dynamic runtime dispatch is supported (compile-time optimization not yet implemented)
- Some advanced StringZilla features may not be exposed yet
- Error handling follows Zig conventions rather than C-style error codes

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `zig build test`
6. Submit a pull request

## License

This project follows the same license as the original StringZilla library. Please refer to the [StringZilla repository](https://github.com/ashvardanian/StringZilla) for license information.

## Acknowledgments

- [StringZilla](https://github.com/ashvardanian/StringZilla) by Ash Vardanian for the high-performance C implementation
- The Zig community for the excellent language and tooling

## Related Projects

- [StringZilla](https://github.com/ashvardanian/StringZilla) - Original C library
