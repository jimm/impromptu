#!/usr/bin/env ruby

require 'rubygems'
require 'midilib'

def unique_file_name_from(seq_name)
  if File.exist?(seq_name)
    seq_name = seq_name + ' a'
    while File.exist?(seq_name)
      seq_name = seq_name.succ
    end
  end
  seq_name
end

seq = MIDI::Sequence.new()
secs_per_beat = 1.0 / (seq.beats_per_minute * 60.0)
File.open(ARGV[0], 'rb') { | file | seq.read(file) }
class << seq
  def delta_to_secs(delta)
    delta / (self.beats_per_minute * self.ppqn / 60.0)
  end
end


File.open(unique_file_name_from(seq.name), 'w') { | f |
  f.puts '('
  seq.each { | track |
    starts = Array.new
    time_from_starts = Array.new
    pitches = Array.new
    volumes = Array.new
    durations = Array.new
    playing_notes = Hash.new    # key = index into durations, val = start-time
    track.each { | e |
      if e.note_on?
        starts << seq.delta_to_secs(e.delta_time)
        time_from_starts << e.time_from_start
        pitches << e.note
        volumes << e.velocity
        playing_notes[e.note] = durations.length
        durations << 1          # will be replaced when note off seen
      elsif e.note_off?
        i = playing_notes[e.note]
        durations[i] = seq.delta_to_secs(e.time_from_start - time_from_starts[i])
      end
    }
    next unless starts.length > 0
    f.puts '('
    f.puts '(' + starts.join(' ') + ')'
    f.puts '(' + pitches.join(' ') + ')'
    f.puts '(' + volumes.join(' ') + ')'
    f.puts '(' + durations.join(' ') + ')'
    f.puts ')'
  }
  f.puts ')'
}
