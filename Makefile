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
    ifeq ($(UNAME_S),Linux)
        OS_ID := linux
    endif
    ifeq ($(UNAME_S),Darwin)
        OS_ID := osx
    endif
    UNAME_P := $(shell uname -p)
    ifeq ($(UNAME_P),x86_64)
        ARCH_ID := x64
    endif
    ifneq ($(filter %86,$(UNAME_P)),)
        ARCH_ID := x64
    endif
    ifneq ($(filter arm%,$(UNAME_P)),)
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
	
.PHONY: run
run:
	dotnet run --project Marksman -- $(ARGS)

.PHONY: fmt
fmt: setup
	dotnet fantomas Marksman
ifneq ($(OS_ID),win)
	xmllint Marksman/Marksman.fsproj -o Marksman/Marksman.fsproj
endif

.PHONY: publish
publish:
	dotnet publish -c Release -r $(RID) --self-contained true \
		-p:PublishSingleFile=true \
		-p:PublishTrimmed=true \
		-p:DebugType=embedded \
		-p:EnableCompressionInSingleFile=true \
		Marksman/Marksman.fsproj
		
.PHONY: publishTo
publishTo:
	dotnet publish -c Release -r $(RID) --self-contained true \
		-p:PublishSingleFile=true \
		-p:PublishTrimmed=true \
		-p:DebugType=embedded \
		-p:EnableCompressionInSingleFile=true \
		Marksman/Marksman.fsproj \
		-o $(DEST)
		
# Install the binary to $HOME/.local/bin folder
.PHONY: install
ifeq ($(OS_ID),win)
install: publish
	cmd /C scripts\install.bat
else
install: publish
	mkdir -p $${HOME}/.local/bin
	cp -f Marksman/bin/Release/net6.0/$(RID)/publish/marksman $${HOME}/.local/bin
endif
