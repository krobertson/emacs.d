#!/usr/bin/env ruby
# -*- ruby -*-

require 'rake/clean'

#EMACS_CMD='/Applications/Aquamacs.app/Contents/MacOS/Aquamacs'
EMACS_CMD="/Applications/Emacs.app/Contents/MacOS/Emacs"

EL_FILES = FileList['**/*.el']
ELC_FILES = EL_FILES.ext('.elc')

CLOBBER.include(ELC_FILES)
CLOBBER.include('html')

task :elc => ELC_FILES

rule ".elc" => ".el" do |t|
  sh "#{EMACS_CMD} -batch -f batch-byte-compile #{t.source}" do |ok, status|
    puts "Compile failed: #{status}" unless ok
  end
end

task :install do
  current_path = File.expand_path '.'
  %x{ ln -snf #{current_path}/init.el ~/.emacs }
  %x{ ln -snf #{current_path} ~/.emacs.d }
end

