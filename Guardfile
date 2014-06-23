# ruby script to run build and run tests every time a source or test file is modified
# required: ruby language
# gem install guard guard-shell
guard :shell do
  watch(%r{.*\.cabal$}) do
    `cabal build`
  end

  watch(%r{src/.*hs}) do
    `cabal build`
  end
end

# Add files and commands to this file, like the example:
#   watch(%r{file/path}) { `command(s)` }
#
guard :shell do
  watch(/(.*).txt/) {|m| `tail #{m[0]}` }
end

# Add files and commands to this file, like the example:
#   watch(%r{file/path}) { `command(s)` }
#
guard :shell do
  watch(/(.*).txt/) {|m| `tail #{m[0]}` }
end
