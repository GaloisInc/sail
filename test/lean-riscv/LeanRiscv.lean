import Std.Data.ExtHashMap.Basic

import ELFSage
import LeanRV64DExecutable
import LeanRV64DExecutable.Sail.Sail
import LeanRV64DExecutable.Step

open Register

def readElf (elfFilepath : System.FilePath) : IO (Except String RawELFFile) := do
  let bytes <- IO.FS.readBinFile elfFilepath
  pure (mkRawELFFile? bytes)

def readElf32 (elfFilepath : System.FilePath) : IO (Except String ELF32File) := do
  let bytes <- IO.FS.readBinFile elfFilepath
  match mkRawELFFile? bytes with
  | .error warning => do
    pure (.error warning)
  | .ok (.elf32 elf) => do
    -- IO.println s!"{repr elf}"
    pure (.ok elf)
  | .ok (.elf64 _elf) => do
    pure (.error "64 bit ELF file not supported")

inductive MachineBits where
  | B32
  | B64

def DEFAULT_RSTVEC := 0x00001000

def initializeMemory (_size: MachineBits) (elf : ELF64File) : Std.ExtHashMap Nat (BitVec 8) :=
  -- From: sail-riscv/c_emulator/riscv_sim.cpp
  --
  -- let RST_VEC_SIZE : UInt32 := 8
  -- let is_32bit_model := match size with
  --   | .B32 => true
  --   | .B64 => false
  -- let entry : UInt64 := sorry
  -- -- Little endian
  -- let reset_vec : List UInt32 := [
  --   0x297,                                                     -- auipc  t0,0x0
  --   (0x28593 : UInt32) + (RST_VEC_SIZE * (4 : UInt32) <<< 20), -- addi   a1, t0, &dtb
  --   0xf1402573,                                                -- csrr   a0, mhartid
  --   if is_32bit_model then 0x0182a283                          -- lw     t0,24(t0)
  --                     else 0x0182b283,                         -- ld     t0,24(t0)
  --   0x28067,                                                   -- jr     t0
  --   0,
  --   UInt64.toUInt32 (entry &&& 0xffffffff),
  --   UInt64.toUInt32 (entry >>> 32)
  -- ]
  -- let rv_rom_base := DEFAULT_RSTVEC

  let update_mem_segment mem first_addr body :=
    let addrs := Array.range' first_addr (Array.size body)
    Array.foldl (λ mem (addr, byte) =>
      if mem.contains addr then
        panic s!"Address {addr} is already written to!"
      else
        mem.insert addr byte.toBitVec
    ) mem (Array.zip addrs body)

  -- Handle interpreted_segments
  let mem'' := List.foldl (λ mem (_header, inst) =>
          -- TODO(JP): Is this address correct?
          update_mem_segment mem inst.segment_base inst.segment_body.data
        ) default elf.interpreted_segments
  -- Handle interpreted_sections
  let mem' := mem''
  -- let mem' := List.foldl (λ mem (_header, inst) =>
  --         -- TODO(JP): Is this address correct?
  --         update_mem_segment mem inst.section_offset inst.section_body.data
  --       ) mem'' elf.interpreted_sections
  -- Handle bits_and_bobs
  let mem := List.foldl (λ mem (addr, data) =>
          update_mem_segment mem addr data.data
        ) mem' elf.bits_and_bobs

  mem

def is_tohost (s : ELF64SectionHeaderTableEntry × InterpretedSection) : Bool :=
  s.snd.section_name_as_string == some ".tohost"

def initializeRegisters (elf: ELF64File) :=
  open LeanRV64DExecutable.Functions in
  open Sail in
  do
  -- -- TODO: initialize register properly
  -- let emptyRegs := Std.ExtDHashMap.emptyWithCapacity
  -- let regs := emptyRegs.insert PC (elf.file_header.e_entry:UInt32).toBitVec
  -- regs
  -- dbg_trace (repr elf)
  let tohost_addr_m := (elf.interpreted_sections.find? is_tohost).map (λ (s: ELF64SectionHeaderTableEntry × InterpretedSection) => s.snd.section_addr)
  match tohost_addr_m with
  | none => do
      panic ".tohost address not found in ELF"
  | some tohost_addr => do
    writeReg PC (elf.file_header.e_entry:UInt64).toBitVec
    writeReg htif_tohost (tohost_addr:UInt64).toBitVec
    enable_htif (tohost_addr:UInt64).toBitVec

def my_main (elf: ELF64File) :=
  open LeanRV64DExecutable.Functions in
  open Sail in
  do
  -- monadLift (IO.print "TEST")
  -- let _ <- pure (unsafeIO (IO.print "TEST"))
  dbg_trace "In my_main!"
  -- print_effect
  -- pure (print_bits "PC = " (← readReg PC))
  print_bits_effect "PC = " (← readReg PC)
  print_bits_effect "htif_tohost = " (← readReg htif_tohost)
  sailTryCatch (do
      init_model ""
      cycle_count ()
      initializeRegisters elf
      print_bits_effect "PC = " (← readReg PC)
      print_bits_effect "htif_tohost = " (← readReg htif_tohost)
      loop ()
  ) (λ the_exception ↦
    match the_exception with
      | .Error_not_implemented s => (pure (print_string "Error: Not implemented: " s))
      | .Error_internal_error () => (pure (print "Error: internal error"))
  )


def runElf64 (elf : ELF64File) : IO UInt32 :=
  open Sail in
  open LeanRV64DExecutable.Functions in
  let mem := initializeMemory MachineBits.B64 elf
  let regs := Std.ExtDHashMap.emptyWithCapacity -- initializeRegisters elf
  let initialState := ⟨regs, (), mem, default, default, default⟩
  main_of_sail_main initialState $ fun () => do
    initializeRegisters elf
    sail_model_init ()
    my_main elf
  -- main_of_sail_main initialState my_main
