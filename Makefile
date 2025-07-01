BUILD=target
EXMPL=examples

$(BUILD)/debug/my-lang.exe:
	cargo build

$(EXMPL)/%.asm: $(BUILD)/debug/my-lang.exe
	$(BUILD)/debug/my-lang.exe -t windows $(EXMPL)/$*.txt -o $(EXMPL)/$*.asm

$(EXMPL)/%.obj: $(EXMPL)/%.asm
	nasm -fwin64 $(EXMPL)/$*.asm

$(EXMPL)/%.exe: $(EXMPL)/%.obj
	cc $(EXMPL)/$*.obj -o $(EXMPL)/$*.exe