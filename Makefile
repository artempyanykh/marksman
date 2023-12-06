# Allow packagers to override PREFIX with their distribution standard
PREFIX?=${HOME}/.local

# Do magic to turn make arguments into command line arguments for executable invocation
ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

# Do some magic to figure out host's OS and ARCH.
# This will be used later to build RID for publishing of a self-contained binary.
OS_ID :=
ARCH_ID :=
ifeq ($(OS),Windows_NT)
	OS_ID := win
	ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
		ARCH_ID := x64
	else
		ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
			ARCH_ID := x64
		endif
		ifeq ($(PROCESSOR_ARCHITECTURE),x86)
			ARCH_ID := x32
		endif
	endif
else
	UNAME_S := $(shell uname -s)
	TARGET := $(shell uname -m)
	
	ifeq ($(UNAME_S),Linux)
		OS_ID := linux
	endif

	ifeq ($(UNAME_S),Darwin)
		OS_ID := osx
	endif

	ifeq ($(TARGET),x86_64)
		ARCH_ID := x64
	endif

	ifeq ($(TARGET),x86)
		ARCH_ID := x86
	endif

	ifeq ($(TARGET),arm)
		ARCH_ID := arm
	endif
	
	ifeq ($(TARGET),arm64)
		ARCH_ID := arm64
	endif

	ifeq ($(TARGET),aarch64)
		ARCH_ID := arm64
	endif
endif

RID := $(OS_ID)-$(ARCH_ID)

.PHONY: setup
setup:
	dotnet tool restore

.PHONY: check
check: setup
	dotnet fantomas --check Marksman
	dotnet fsi scripts/silent-lint.fsx

.PHONY: build
build:
	dotnet build Marksman/Marksman.fsproj

.PHONY: test
test:
	dotnet test

.PHONY: clean
clean:
	dotnet clean
	
.PHONY: run
run:
	dotnet run --project Marksman -- $(ARGS)

.PHONY: bench
bench:
	dotnet run -c Release --project Benchmarks -- $(ARGS)

.PHONY: fmt
fmt: setup
	dotnet fantomas Marksman
	dotnet fantomas Tests
	dotnet fantomas Benchmarks
ifneq ($(OS_ID),win)
	xmllint Marksman/Marksman.fsproj -o Marksman/Marksman.fsproj
endif

.PHONY: publish
publish:
	dotnet publish -c Release -r $(RID) --self-contained true \
		-p:PublishSingleFile=true \
		-p:PublishTrimmed=true \
		-p:TrimMode=partial \
		-p:DebugType=embedded \
		-p:EnableCompressionInSingleFile=true \
		-p:UseAppHost=true \
		Marksman/Marksman.fsproj
		
.PHONY: publishTo
publishTo:
	dotnet publish -c Release -r $(RID) --self-contained true \
		-p:PublishSingleFile=true \
		-p:PublishTrimmed=true \
		-p:TrimMode=partial \
		-p:DebugType=embedded \
		-p:EnableCompressionInSingleFile=true \
		-p:VersionString=$(VERSIONSTRING) \
		-p:UseAppHost=true \
		Marksman/Marksman.fsproj \
		-o $(DEST)
		
.PHONY: macosUniversalBinary
macosUniversalBinary:
	$(MAKE) publishTo DEST=$(DEST) RID=osx-x64
	mv $(DEST)/marksman $(DEST)/marksman-x64
	$(MAKE) publishTo DEST=$(DEST) RID=osx-arm64
	mv $(DEST)/marksman $(DEST)/marksman-arm64
	lipo -create -output $(DEST)/marksman $(DEST)/marksman-x64 $(DEST)/marksman-arm64

# Install the binary to $HOME/.local/bin folder
.PHONY: install
ifeq ($(OS_ID),win)
install: publish
	cmd /C scripts\install.bat
else
install: publish
	mkdir -p $(PREFIX)/bin
	install -m755 Marksman/bin/Release/net8.0/$(RID)/publish/marksman $(PREFIX)/bin
endif

.PHONY: uninstall
uninstall:
	rm -rf $(PREFIX)/bin/marksman

.DEFAULT_GOAL := build
