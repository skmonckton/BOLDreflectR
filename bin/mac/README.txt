For security reasons, macOS might prevent these binaries from running.
If you get a message saying that "Apple cannot check it for malicious software",
run this command once:
$ sudo xattr -r -d com.apple.quarantine ./avifenc ./avifdec ./avifgainmaputil
See also https://github.com/AOMediaCodec/libavif/issues/2460
