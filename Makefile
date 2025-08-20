BUILD=target
EXMPL=examples

$(BUILD)/debug/mapla.exe:
	cargo build

$(EXMPL)/%.asm: $(BUILD)/debug/mapla.exe
	$(BUILD)/debug/mapla.exe -t windows $(EXMPL)/$*.txt -o $(EXMPL)/$*.asm

$(EXMPL)/%.obj: $(EXMPL)/%.asm
	nasm -fwin64 $(EXMPL)/$*.asm

$(EXMPL)/%.exe: $(EXMPL)/%.obj
	cc $(EXMPL)/$*.obj -o $(EXMPL)/$*.exe