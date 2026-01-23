BUILD=target
EXMPL=examples

# Windows Builds

$(BUILD)/debug/mapla.exe:
	cargo build

$(EXMPL)/%.win.asm: $(BUILD)/debug/mapla.exe
	$(BUILD)/debug/mapla.exe -t windows $(EXMPL)/$*.txt -o $(EXMPL)/$*.win.asm

$(EXMPL)/%.win.obj: $(EXMPL)/%.win.asm
	nasm -fwin64 $(EXMPL)/$*.win.asm -o $(EXMPL)/$*.win.obj

$(EXMPL)/%.exe: $(EXMPL)/%.win.obj
	cc $(EXMPL)/$*.win.obj -o $(EXMPL)/$*.exe $(CFLAGS)

# Linus Builds

$(BUILD)/debug/mapla:
	cargo build

$(EXMPL)/%.linux.asm: $(BUILD)/debug/mapla
	$(BUILD)/debug/mapla -t linux $(EXMPL)/$*.txt -o $(EXMPL)/$*.linux.asm

$(EXMPL)/%.linux.obj: $(EXMPL)/%.linux.asm
	nasm -felf64 $(EXMPL)/$*.linux.asm -o $(EXMPL)/$*.linux.obj

$(EXMPL)/%: $(EXMPL)/%.linux.obj
	cc $(EXMPL)/$*.linux.obj -o $(EXMPL)/$* -no-pie -z noexecstack $(CFLAGS)