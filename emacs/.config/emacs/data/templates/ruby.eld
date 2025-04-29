ruby-mode

(shb "#!/usr/bin/env ruby" n)

(inl & n "require 'bundler/inline'\n\ngemfile do
  source 'https://rubygems.org'\nend" o n)

(hdoc "<<~" (p "EOF" name) n> "  " q n> name)

(r "require '" q "'")
(rr "require_relative '" q "'")

