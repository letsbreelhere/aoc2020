require 'ostruct'

input = File.read(File.expand_path './inputs/4.txt')

entries = input.split(/\n\n/)

def parse(entry)
  pairs = entry.split(/\s+/)
  hash = Hash[pairs.map { |s| s.split(':') }]
  OpenStruct.new(hash)
end

def height_valid?(hgt)
  match = hgt.match(/(\d+)in|(\d+)cm/)
  return false unless match

  if match[1]
    (59..76).cover?(match[1].to_i)
  elsif match[2]
    (150..193).cover?(match[2].to_i)
  end
end

def valid?(e)
  all_present = %w[byr iyr eyr hgt hcl ecl pid].all? { |k| e[k] }
  return false unless all_present

  byr = (1920..2002).cover?(e.byr.to_i)
  iyr = (2010..2020).cover?(e.iyr.to_i)
  eyr = (2020..2030).cover?(e.eyr.to_i)
  hgt = height_valid?(e.hgt)
  hcl = e.hcl.match(/^#[0-9a-f]{6}$/)
  ecl = %w[amb blu brn gry grn hzl oth].include?(e.ecl)
  pid = e.pid.match(/^\d{9}$/)

  [
    byr,
    iyr,
    eyr,
    hgt,
    hcl,
    ecl,
    pid
  ].all?
end

parsed = entries.map { |e| parse(e) }
p(parsed.count { |e| valid?(e) })
