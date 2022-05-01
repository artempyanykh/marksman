# Do magic to turn make arguments into command line arguments for executable invocation
ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

# Do some magic to figure out host's OS and ARCH.
# This will be used later to build RID for publishing of a self-contained binary.
OS :=
ARCH :=
ifeq ($(OS),Windows_NT)
    OS := win
    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        ARCH := x64
    else
        ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
            ARCH := x64
        endif
        ifeq ($(PROCESSOR_ARCHITECTURE),x86)
            ARCH := x32
        endif
    endif
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        OS := linux
    endif
    ifeq ($(UNAME_S),Darwin)
        OS := osx
    endif
    UNAME_P := $(shell uname -p)
    ifeq ($(UNAME_P),x86_64)
        ARCH := x64
    endif
    ifneq ($(filter %86,$(UNAME_P)),)
        ARCH := x32
    endif
    ifneq ($(filter arm%,$(UNAME_P)),)
        ARCH := arm64
    endif
endif

RID := $(OS)-$(ARCH)

.PHONY: build
build:
	dotnet build Marksman/Marksman.fsproj
	
.PHONY: run
run:
	dotnet run --project Marksman -- $(ARGS)

.PHONY: publish
publish:
	dotnet publish -c Release -r $(RID) --self-contained true \
		-p:PublishSingleFile=true \
		-p:PublishTrimmed=true \
		-p:DebugType=embedded \
		-p:EnableCompressionInSingleFile=true \
		Marksman/Marksman.fsproj
		
.PHONY: install
install: publish
	mkdir -p $${HOME}/.local/bin
	cp -f Marksman/bin/Release/net6.0/$(RID)/publish/Marksman $${HOME}/.local/bin