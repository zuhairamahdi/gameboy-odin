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

last_cycles : u32

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
instructions : []Instruction = {}
CB_instructions : []Instruction = {}
Cycles : [256]u8 = {
	4, 6, 4, 4, 2, 2, 4, 4, 10, 4, 4, 4, 2, 2, 4, 4,  // 0x0_
	2, 6, 4, 4, 2, 2, 4, 4, 4,  4, 4, 4, 2, 2, 4, 4,  // 0x1_
	0, 6, 4, 4, 2, 2, 4, 2, 0,  4, 4, 4, 2, 2, 4, 2,  // 0x2_
	4, 6, 4, 4, 6, 6, 6, 2, 0,  4, 4, 4, 2, 2, 4, 2,  // 0x3_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0x4_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0x5_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0x6_
	4, 4, 4, 4, 4, 4, 2, 4, 2,  2, 2, 2, 2, 2, 4, 2,  // 0x7_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0x8_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0x9_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0xa_
	2, 2, 2, 2, 2, 2, 4, 2, 2,  2, 2, 2, 2, 2, 4, 2,  // 0xb_
	0, 6, 0, 6, 0, 8, 4, 8, 0,  2, 0, 0, 0, 6, 4, 8,  // 0xc_
	0, 6, 0, 0, 0, 8, 4, 8, 0,  8, 0, 0, 0, 0, 4, 8,  // 0xd_
	6, 6, 4, 0, 0, 8, 4, 8, 8,  2, 8, 0, 0, 0, 4, 8,  // 0xe_
	6, 6, 4, 2, 0, 8, 4, 8, 6,  4, 8, 2, 0, 0, 4, 8   // 0xf_
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


detect_banking_mode :: proc() {
    // Detect MBC type from ROM header
    switch rom[0x147] {
    case 1, 2, 3:  // MBC1 variants
        mbc1 = true
        fmt.println("Using MBC1")
    
    case 5, 6:  // MBC2 variants
        mbc2 = true
        fmt.println("Using MBC2")
    
    case:  // No MBC or unsupported
        // Default case does nothing
    }

    // Print RAM bank information
    ram_banks := rom[0x148]
    fmt.printf("Number of RAM banks: %d\n", ram_banks)
}

print_registers :: proc() {
    // Register values
    fmt.printf("af: %04x\n",   registers.af)
    fmt.printf("bc: %04x\n",   registers.bc)
    fmt.printf("de: %04x\n",   registers.de)
    fmt.printf("hl: %04x\n",   registers.hl)
    fmt.printf("sp: %04x\n",   registers.sp)
    fmt.printf("pc: %04x\n",   registers.pc)

    // Stack value (little-endian)
    stack_value := u16(memory[registers.sp + 1]) << 8 | 
                   u16(memory[registers.sp])
    fmt.printf("Stack Value: %04x\n", stack_value)

    // Current instruction
    current_op := memory[registers.pc]
    fmt.printf("0x%04x: %s (0x%02x)\n", 
               registers.pc, 
               instructions[current_op].name, 
               current_op)

    // Additional state
    fmt.printf("IME: %x\n", IME)
    fmt.printf("operand16: %04x\n", Operand16)
    fmt.printf("operand8: %02x\n", Operand8)
}

setup_color_palette :: proc() {
    palette_byte := memory[0xFF47]
    
    for i in 0..<4 {
        // Extract 2-bit color value (shifts 0, 2, 4, 6 bits)
        color := (palette_byte >> (u8(i) * 2)) & 0b11
        
        // Set RGB values based on color index
        switch color {
        case 0:  // White
            color_palette[i] = RGB{255, 255, 255}
        case 1:  // Light gray
            color_palette[i] = RGB{180, 180, 180}
        case 2:  // Dark gray
            color_palette[i] = RGB{110, 110, 110}
        case 3:  // Black
            color_palette[i] = RGB{0, 0, 0}
        }
    }
}

initialize_sdl :: proc() {
    // Initialize SDL
    if sdl2.Init(sdl2.INIT_VIDEO) != 0 {
        fmt.eprintln("SDL initialization failed:", sdl2.GetError())
        os.exit(1)
    }

    // Create window and renderer
    window = sdl2.CreateWindow(
        "GameBoy Emulator",
        sdl2.WINDOWPOS_CENTERED,
        sdl2.WINDOWPOS_CENTERED,
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        sdl2.WINDOW_SHOWN
    )
    if window == nil {
        fmt.eprintln("Window creation failed:", sdl2.GetError())
        os.exit(1)
    }

    renderer = sdl2.CreateRenderer(window, -1, sdl2.RENDERER_ACCELERATED)
    if renderer == nil {
        fmt.eprintln("Renderer creation failed:", sdl2.GetError())
        os.exit(1)
    }

    // Set window icon
    icon = sdl2.LoadBMP("icon.bmp")
    if icon != nil {
        sdl2.SetWindowIcon(window, icon)
    }

    // Configure renderer
    sdl2.RenderSetLogicalSize(renderer, SCREEN_WIDTH, SCREEN_HEIGHT)
    texture = sdl2.CreateTexture(
        renderer,
        sdl2.PixelFormatEnum.RGB24,
        sdl2.TextureAccess.STREAMING,
        SCREEN_WIDTH,
        SCREEN_HEIGHT
    )

    // Clear to white
    sdl2.SetRenderDrawColor(renderer, 255, 255, 255, 255)
    sdl2.RenderClear(renderer)
    sdl2.RenderPresent(renderer)
}


cpu_cycle :: proc() {
    opcode := read_byte(registers.pc)
    instr := instructions[opcode]

    switch instr.length {
    case 0, 1:
        registers.pc += 1
        instr.fcnPtr()

    case 2:
        Operand8 = read_byte(registers.pc + 1)
        registers.pc += 2
        
        if opcode == 0xCB {
            CB_instructions[Operand8].fcnPtr()
        } else {
            instr.fcnPtr()
        }

    case 3:
        Operand16 = u16(read_byte(registers.pc + 2)) << 8 | 
                    u16(read_byte(registers.pc + 1))
        registers.pc += 3
        instr.fcnPtr()
    }

    cycles := 2 * Cycles[opcode]
    cycle_count += u128(cycles)
    last_cycles = u32(cycles)
}

interrupts :: proc() {
    if !IME do return

    request_flag := read_byte(0xFF0F)
    if request_flag == 0 do return

    enabled_flags := read_byte(0xFFFF)
    
    for bit in 0..<5 {  // Only bits 0-4 are valid interrupt sources
        if (request_flag & (1 << u8(bit))) != 0 && (enabled_flags & (1 << u8(bit))) != 0 {
            do_interrupt(u8(bit))
            // Exit after handling highest priority interrupt
            return
        }
    }
}
push :: proc(value: u16) {
    registers.sp -= 2
    // Write big-endian (high byte first)
    write_byte(u8(value >> 8), registers.sp)
    write_byte(u8(value & 0xFF), registers.sp + 1)
}

pop :: proc() -> u16 {
    // Read big-endian (high byte first)
    value := u16(read_byte(registers.sp)) << 8 | 
             u16(read_byte(registers.sp + 1))
    registers.sp += 2
    return value
}

do_interrupt :: proc(interrupt: u8) {
    IME = false  // Disable further interrupts
    
    // Clear the interrupt flag
    if_flag := read_byte(0xFF0F)
    write_byte(if_flag & ~(1 << interrupt), 0xFF0F)
    
    // Push PC onto stack
    push(registers.pc)
    
    // Jump to interrupt handler
    switch interrupt {
    case 0: registers.pc = 0x40; break;  // VBlank
    case 1: registers.pc = 0x48; break;  // LCD STAT
    case 2: registers.pc = 0x50; break;  // Timer
    case 3: registers.pc = 0x58; break;  // Serial  // Note: Fixed address (was 0x68 in original)
    case 4: registers.pc = 0x60; break;  // Joypad
    case:   panic("Invalid interrupt vector")  // Safety check
    }
}

// Resets (clears) the specified bit in a number
res_bit :: proc(bit: u8, number: u8) -> u8 {
    assert(bit < 8, "Bit position must be 0-7")
    return number & ~(1 << bit)
}

// Sets the specified bit in a number
set_bit :: proc(bit: u8, number: u8) -> u8 {
    assert(bit < 8, "Bit position must be 0-7")
    return number | (1 << bit)
}

load_tiles :: proc() {
    location := u16(0x8000)
    
    for s in 0..<384 {
        for rel_y in 0..<8 {
            lower_byte := read_byte(location + u16(2*rel_y + 16*s))
            upper_byte := read_byte(location + u16(2*rel_y + 16*s + 1))
            
            for rel_x in 0..<8 {
                bit_mask := u8(1 << u32(7 - rel_x))
                color_bit0 := (lower_byte & bit_mask) != 0 ? 1 : 0
                color_bit1 := (upper_byte & bit_mask) != 0 ? 2 : 0
                Tile_Map[s][rel_x][rel_y] = u8(color_bit0) + u8(color_bit1)
            }
        }
    }
}

display_buffer :: proc() {
    // Update texture with frame buffer data
    err := sdl2.UpdateTexture(
        texture,
        nil,  // Entire texture
        raw_data(&frame_buffer),  // Pixel data
        SCREEN_WIDTH * 3  // Pitch (bytes per row)
    )
    if err != 0 {
        fmt.eprintln("Texture update failed:", sdl2.GetError())
        return
    }

    // Render to screen
    sdl2.RenderClear(renderer)
    sdl2.RenderCopy(renderer, texture, nil, nil)
    sdl2.RenderPresent(renderer)
}

shutdown :: proc(exit_code: int = 1) {
    // Cleanup SDL resources (safe to call on nil pointers)
    if texture != nil  do sdl2.DestroyTexture(texture)
    if renderer != nil do sdl2.DestroyRenderer(renderer)
    if window != nil   do sdl2.DestroyWindow(window)
    if icon != nil     do sdl2.FreeSurface(icon)
    
    sdl2.Quit()
    os.exit(exit_code)
}


render_all_tiles :: proc() {
    tiles_per_row := 20  // 160 pixels / 8 pixels per tile
    
    for tile_num in 0..<360 {
        tile_x := tile_num % tiles_per_row
        tile_y := tile_num / tiles_per_row
        
        for x in 0..<8 {
            for y in 0..<8 {
                // Calculate screen positions
                screen_x := tile_x * 8 + x
                screen_y := tile_y * 8 + y
                
                // Get color and render
                color_index := Tile_Map[tile_num][x][y]
                frame_buffer[screen_y][screen_x] = color_palette[color_index]
            }
        }
    }
}


render_tile_map_line :: proc() {
    // Check LCD enable
    lcdc := read_byte(0xFF40)
    if !test_bit(7, lcdc) do return

    currentline := read_byte(0xFF44)
    if currentline >= SCREEN_HEIGHT do return

    // Get scroll/window registers
    scroll_y := read_byte(0xFF42)
    scroll_x := read_byte(0xFF43)
    window_y := read_byte(0xFF4A)
    window_x := read_byte(0xFF4B)

    // Determine addressing mode
    unsig := test_bit(4, lcdc)  // Unsigned tile numbers
    windowing_enabled := test_bit(5, lcdc)

    // Determine tilemap locations
    bg_location := test_bit(3, lcdc) ? 0x9C00 : 0x9800
    win_location := test_bit(6, lcdc) ? 0x9C00 : 0x9800

    // Render background
    y_pos := currentline + scroll_y
    tile_row := (y_pos / 8) % 32 * 32

    pixel: u8 = 0
    for pixel < 160 {
        // Check window condition
        if windowing_enabled && pixel >= window_x && currentline >= window_y {
            break
        }

        x_pos := pixel + scroll_x
        tile_col := (x_pos / 8) % 32

        // Get tile number with signed/unsigned handling
        tile_num := get_tile_number(bg_location + u16(tile_row + tile_col), unsig)

        // Render pixel
        frame_buffer[currentline][pixel] = color_palette[Tile_Map[tile_num][x_pos % 8][y_pos % 8]]
        pixel += 1
    }

    // Render window if enabled
    if windowing_enabled && currentline >= window_y {
        y_pos = currentline - window_y
        tile_row = (y_pos / 8) * 32

        for pixel < 160 {
            x_pos := pixel - window_x
            tile_col := (x_pos / 8) % 32

            tile_num := get_tile_number(win_location + u16(tile_row + tile_col), unsig)

            frame_buffer[currentline][pixel] = color_palette[Tile_Map[tile_num][x_pos % 8][y_pos % 8]]
            pixel += 1
        }
    }
}

render_sprites :: proc() {
    use8x16 := test_bit(2, read_byte(0xFF40)) != false

    for sprite := 0; sprite < 40; sprite += 1 {
        index := sprite * 4
        ypos := read_byte(0xFE00 + u16(index)) - 16
        xpos := read_byte(0xFE00 + u16(index + 1)) - 8
        location := read_byte(0xFE00 + u16(index + 2))
        attributes := read_byte(0xFE00 + u16(index + 3))

        yflip := test_bit(6, attributes)
        xflip := test_bit(5, attributes)

        if ypos == 0 || xpos == 0 || ypos >= 160 || xpos >= 168 {
            continue
        }

        for x := 0; x < 8; x += 1 {
            for y := 0; y < 8; y += 1 {
                if Tile_Map[location][abs(8 * int(xflip) - x)][abs(8 * int(yflip) - y)] != 0 {
                    frame_buffer[(y + ypos) % SCREEN_HEIGHT][(xpos + x) % SCREEN_WIDTH] = 
                        color_palette[Tile_Map[location][abs(8 * int(xflip) - x)][abs(8 * int(yflip) - y)]]
                }
            }
        }
    }
}

handle_input :: proc() {
    if event.type == sdl2.EventType.KEYDOWN {
        key := -1
        switch event.key.keysym.sym {
        case sdl2.Keycode.TAB:
            key = 4
        case sdl2.Keycode.LCTRL:
            key = 5
        case sdl2.Keycode.RETURN:
            key = 7
        case sdl2.Keycode.BACKSLASH:
            key = 6
        case sdl2.Keycode.RIGHT:
            key = 0
        case sdl2.Keycode.LEFT:
            key = 1
        case sdl2.Keycode.UP:
            key = 2
        case sdl2.Keycode.DOWN:
            key = 3
        case:
            key = -1
        }

        if key != -1 {
            key_press(key)
        }
    }
    else if event.type == sdl2.EventType.KEYUP {
        key := -1
        switch event.key.keysym.sym {
        case sdl2.Keycode.TAB:
            key = 4
        case sdl2.Keycode.LCTRL:
            key = 5
        case sdl2.Keycode.RETURN:
            key = 7
        case sdl2.Keycode.BACKSLASH:
            key = 6
        case sdl2.Keycode.RIGHT:
            key = 0
        case sdl2.Keycode.LEFT:
            key = 1
        case sdl2.Keycode.UP:
            key = 2
        case sdl2.Keycode.DOWN:
            key = 3
        case:
            key = -1
        }

        if key != -1 {
            key_release(key)
        }
    }
}
key_press :: proc(key: int) {
    previouslyUnset := false

    if !test_bit(u8(key), joypad_state) {
        previouslyUnset = true
    }

    joypad_state = res(u8(key), joypad_state)

    // Standard or directional button?
    button := key > 3

    // Check which keys game is interested in
    request_interrupt := false
    if button && !test_bit(5, read_byte(0xFF00)) {
        request_interrupt = true
    } else if !button && !test_bit(4, read_byte(0xFF00)) {
        request_interrupt = true
    }

    if request_interrupt && !previouslyUnset {
        set_interrupt(4)
    }
}

set_interrupt :: proc(interrupt: u8) {
    current := read_byte(0xFF0F)
    modified := set_bit(interrupt, current)
    write_byte(modified, 0xFF0F)
}
key_release :: proc(key: int) {
    joypad_state = set(u8(key), joypad_state)
}

// Reset (clear) the specified bit in a number
res :: proc(bit: u8, number: u8) -> u8 {
    bitindex := u8(1) << bit
    return number & (0xFF ~ bitindex)  // Same as XOR in original
}

// Set the specified bit in a number
set :: proc(bit: u8, number: u8) -> u8 {
    bitindex := u8(1) << bit
    return number | bitindex
}


get_tile_number :: proc(addr: u16, unsigned: bool) -> u16 {
    tile_num := u16(read_byte(addr))
    return unsigned ? tile_num : tile_num + 0x100
}
main :: proc() {
    rom_data, ok := read_rom("game.gb")
    if !ok {
        // Handle error
        return
    }
    defer delete(rom_data)
}


Set_Z_Flag:: proc() { 
    registers.af.f = registers.af.f | 0x80
}

Set_N_Flag::proc() {
	registers.af.f = registers.af.f | 0x40
}

Set_H_Flag::proc() { 
	registers.af.f = registers.af.f | 0x20
}

Set_C_Flag::proc() {
	registers.af.f = registers.af.f | 0x10
}

Clear_Z_Flag::proc() { 
	registers.af.f = registers.af.f & 0x7F
}

Clear_N_Flag::proc() { 
	registers.af.f = registers.af.f & 0xBF; 
}

Clear_H_Flag::proc() {
	registers.af.f = registers.af.f & 0xDF; 
}

Clear_C_Flag::proc() {
	registers.af.f = registers.af.f & 0xEF; 
}