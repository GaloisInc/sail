import LeanRiscv

def main (args : List String) : IO UInt32 := do
  if args.length != 2 then do
    IO.println "usage: run-riscv-lean <elf_file>"

    pure 255
  else do
    -- Parse input elf file.
    let elfE <- readElf args[1]!
    match elfE with
    | Except.error err => do
      IO.println "Failed to parse elf file:"
      IO.println err

      pure 255
      
    | Except.ok (.elf64 elf) => do
      -- Run program
      runElf64 elf

      -- IO.println "64 bit ELF file not supported"

      -- pure 255
    | Except.ok (.elf32 _elf) => do
      -- -- Run program
      -- runElf32 elf

      IO.println "32 bit ELF file not supported"

      pure 255
