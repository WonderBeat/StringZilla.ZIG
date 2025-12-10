# StringZilla.ZIG

[![CI](https://github.com/WonderBeat/StringZilla.ZIG/workflows/CI/badge.svg)](https://github.com/WonderBeat/StringZilla.ZIG/actions)
[![Latest Release](https://img.shields.io/github/release/WonderBeat/StringZilla.ZIG.svg)](https://github.com/WonderBeat/StringZilla.ZIG/releases/latest)
[![License](https://img.shields.io/github/license/WonderBeat/StringZilla.ZIG.svg)](https://github.com/WonderBeat/StringZilla.ZIG/blob/main/LICENSE)
[![Zig Version](https://img.shields.io/badge/zig-0.15.2+-yellow.svg)](https://ziglang.org/download/)

A high-performance Zig wrapper for [StringZilla](https://github.com/ashvardanian/StringZilla) - a SIMD-accelerated string processing library designed for massive datasets

<img width="800" alt="image" src="https://github.com/user-attachments/assets/5cee8781-92ca-44ea-a887-26ffe7245c49" />

## Overview

StringZilla.ZIG provides idiomatic Zig bindings to the StringZilla C library

## Features

- **String Search**: `find()`, `rfind()`, `find_byteset()`, `find_byte_from()`
- **Hashing**: `bytesum()`, `hash()`, `hmac_sha256()`
- **UTF-8 Support**: Character counting and Unicode-aware operations
- **Memory Operations**: `copy()`, `move()`, `fill()`, `lookup()`
- **Character Set Operations**: `Byteset` for efficient pattern matching

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

// HMAC-SHA256 
const hmac = stringzilla.hmac_sha256("secret-key", "message");
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

// Move memory with overlap handling (new improved API)
stringzilla.moveMemory(buffer[10..60], buffer[0..50]);

```

### UTF-8 Unpacking

```zig
const utf8_text = "Hello 🌍 世界";
var runes: [16]u32 = undefined;

// Unpack UTF-8 to UTF-32 runes with enhanced API
const result = stringzilla.utf8_unpack_chunk(utf8_text, &runes);

std.debug.print("Consumed {} bytes, unpacked {} runes\n", .{ 
    result.bytes_consumed, result.runes_unpacked 
});

// Access individual runes
for (0..result.runes_unpacked) |i| {
    std.debug.print("Rune {}: {X}\n", .{ i, runes[i] });
}
```

### Character Set Manipulation

```zig
const text = "The quick brown fox jumps over the lazy dog";

// Create a byteset for pattern matching
var pattern = stringzilla.Byteset{};
pattern.add('A');
pattern.add('B');
pattern.add('C');

// Create from bytes 
const vowels = stringzilla.Byteset.from_bytes("aeiou");
const consonants = stringzilla.Byteset.from_bytes("bcdfghjklmnpqrstvwxyz");

// Find characters in sets
if (stringzilla.find_byteset(text, vowels)) |pos| {
    std.debug.print("First vowel at position {}\n", .{pos});
}

if (stringzilla.find_byteset(text, consonants)) |pos| {
    std.debug.print("First consonant at position {}\n", .{pos});
}

// Alternative: find from byte slice
if (stringzilla.find_byte_from(text, "xyz")) |pos| {
    std.debug.print("Found 'x', 'y', or 'z' at position {}\n", .{pos});
}

// Find characters NOT in set
if (stringzilla.find_byte_not_from(text, "aeiou ")) |pos| {
    std.debug.print("First non-vowel/non-space at position {}\n", .{pos});
}
```


### UTF-8 Processing

```zig
const utf8_text = "Hello, 世界! 🌍";

// Count Unicode characters
const char_count = stringzilla.count_utf8(utf8_text);
std.debug.print("Unicode characters: {}\n", .{char_count});

// Case-insensitive search with Unicode (returns Range)
if (stringzilla.utf8_case_insensitive_find(utf8_text, "WORLD")) |range| {
    const found_text = range.slice(utf8_text);
    std.debug.print("Case-insensitive match: '{s}'\n", .{found_text});
}

// Case-insensitive Unicode search (Russian text example)
const russian_text = "Привет МИР, привет мир!";
if (stringzilla.utf8_case_insensitive_find(russian_text, "мир")) |range| {
    const found_word = range.slice(russian_text);
    std.debug.print("Found Russian word: '{s}'\n", .{found_word});
}

// Find newline characters
if (stringzilla.find_newline_utf8(utf8_text)) |newline_slice| {
    std.debug.print("Newline found: '{any}'\n", .{newline_slice});
}

// Find whitespace (UTF-8 aware) 
if (stringzilla.find_whitespace_utf8(utf8_text)) |whitespace_slice| {
    std.debug.print("Whitespace found: '{any}'\n", .{whitespace_slice});
}
```

## Recent API Improvements

### 🔥 **Enhanced Ergonomics**

**Range Struct**: Replace anonymous tuples with a proper `Range` struct:
```zig
// Before: Anonymous struct
const result = utf8_case_insensitive_find(text, "world");
const slice = text[result.?.offset .. result.?.offset + result.?.length];

// After: Clean Range API  
const result = utf8_case_insensitive_find(text, "world");
const slice = result.?.slice(text);
```

**Utf8UnpackResult**: Structured UTF-8 unpacking results:
```zig
const result = utf8_unpack_chunk(text, &runes);
```

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
