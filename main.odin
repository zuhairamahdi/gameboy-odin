package main

import "core:fmt"
import "vendor:sdl2"
import "core:os"
import "core:mem"

SCREEN_WIDTH :: 160
SCREEN_HEIGHT :: 144

CLOCKSPEED :: 4194304
CYCLES_PER_FRAME :: 69905

timer_count : u32 = 0
curr_clock_speed : u16 = 1024
divider_count : u32 = 0

Operand8 : u8
Operand16 : u16

cycle_count : u128

last_cycle : u32

IME : bool = false

scanline_count: u32 = 0

Tile_Map : [384][8][8]u8

RGB :: struct {
    red: u8,
    green: u8,
    blue: u8,
}

frame_buffer:    [SCREEN_HEIGHT][SCREEN_WIDTH]RGB
color_palette:   [4]RGB

joypad: u8 = 0xFF
joypad_state: u8 = 0xFF
memory: [65536]u8 = {}
// uint8_t* rom;
// uint8_t* boot_rom;
rom : [^]u8
boot_rom : [^]u8
enable_boot : bool = true
bank_offset : u16 = 0

mbc1 : bool = false
mbc2 : bool = false


Registers :: struct {
    af: struct #raw_union {
        using _: struct {
            f: u8,  // Flag Register. [z,n,h,c,0,0,0,0]
            a: u8,
        },
        af: u16,
    },

    bc: struct #raw_union {
        using _: struct {
            c: u8,
            b: u8,
        },
        bc: u16,
    },

    de: struct #raw_union {
        using _: struct {
            e: u8,
            d: u8,
        },
        de: u16,
    },

    hl: struct #raw_union {
        using _: struct {
            l: u8,
            h: u8,
        },
        hl: u16,
    },

    sp: u16,  // Stack pointer.
    pc: u16,  // Program counter.
}

registers: Registers

Instruction :: struct {
    name: string,
    length: i32,
    fcnPtr: proc()
}

event : sdl2.Event
renderer : ^sdl2.Renderer
window : ^sdl2.Window
texture : ^sdl2.Texture
icon : ^sdl2.Surface

read_rom :: proc(filename: string) -> (rom_data: []u8, success: bool) {
    data, ok := os.read_entire_file(filename)
    if !ok {
        fmt.eprintln("Invalid Rom File!")
        return nil, false
    }

    // Make a copy of the data
    rom_data = make([]u8, len(data))
    copy(rom_data, data)

    fmt.printf("Loaded %s\n", filename)
    return rom_data, true
}



load_bootrom :: proc(filename: string) -> (boot_data: []u8, success: bool) {
    data, ok := os.read_entire_file(filename)
    if !ok {
        fmt.eprintln("Invalid Bootrom File!")
        return nil, false
    }

    boot_data = make([]u8, len(data))
    copy(boot_data, data)

    fmt.printf("Loaded %s\n", filename)
    return boot_data, true
}

read_byte :: proc(location: u16) -> u8 {
    // If in Bootrom
    if enable_boot && location < 0x100 {
        return boot_rom[location]
    }

    // Base Rom Read
    if location < 0x4000 {
        return rom[location]
    }

    // Rom Bank Read
    if location < 0x8000 {
        bank_offset := u32(bank_offset)  // Prevent overflow
        return rom[u32(location) + bank_offset * 0x4000]
    }

    // Key interrupt
    if location == 0xFF00 {
        return key_state()
    }

    return memory[location]
}

key_state :: proc() -> u8 {
    res := memory[0xFF00] ~ 0xFF  // XOR with 0xFF (equivalent to inversion)

    // Are we interested in the standard buttons?
    if !test_bit(4, res) {
        top_joypad := (joypad_state >> 4) | 0xF0
        res &= top_joypad
    } else if !test_bit(5, res) {
        bottom_joypad := (joypad_state & 0xF) | 0xF0
        res &= bottom_joypad
    }
    return res
}
test_bit :: proc(bit: u8, number: u8) -> bool {
    return (number & (1 << bit)) != 0
}

write_byte :: proc(data: u8, location: u16) {
    // Rom Bank Set
    if 0x2000 <= location && location <= 0x3FFF {
        bank_offset = u16(data - 1)
    }
    else if location < 0x8000 {
        return
    }
    else if location == 0xFF46 {
        dma_transfer(data)
    }
    else if location == 0xFF44 {
        memory[0xFF44] = 0
    }
    else if location == 0xFF04 {
        memory[0xFF04] = 0
        divider_count = 0
    }
    else {
        memory[location] = data
    }
}


dma_transfer :: proc(data: u8) {
    address := u16(data) << 8
    for i in 0..<0xA0 {
        value := read_byte(address + u16(i))
        write_byte(value, 0xFE00 + u16(i))
    }
}

main :: proc() {
    rom_data, ok := read_rom("game.gb")
    if !ok {
        // Handle error
        return
    }
    defer delete(rom_data)
}