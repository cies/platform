#!/usr/bin/env ruby

# simple vote class
class Vote
  attr_accessor :casted_at, :vector#, :casted_by
  def initialize(vector, casted_at = Time.now)
    raise "Bad vector (not a Fixnum or Float)" unless [Fixnum, Float].include? vector.class
    @vector, @casted_at = vector, casted_at
  end
end

# calculates the signed surface under a line from +prev_saldo+ to +saldo+ over +duration+
def calculate_surface(duration, prev_saldo, saldo_delta)
  saldo = (prev_saldo + saldo_delta)
  rectangle_height = [prev_saldo.abs, saldo.abs].min
  triangle_height  = saldo.abs
  return (saldo <=> 0) * duration * (rectangle_height + 0.5 * triangle_height)
end

# platform calculation for vectors either 1 or -1
def bool_platform(votes)
  tot_pos = tot_neg = saldo = platform = prev_saldo = 0
  prev_time = last_pivot = nil

  votes.each_with_index do |vote, i|
    raise "Only accepts vectors 1 and -1" unless [1, -1].include? vote.vector
    saldo += vote.vector
    tot_pos += vote.vector if vote.vector > 0
    tot_neg -= vote.vector if vote.vector < 0

    if saldo == 0 or prev_time.nil?
      platform = 0
      last_pivot = vote.casted_at
    else
      duration = vote.casted_at - prev_time
      platform += calculate_surface(duration, prev_saldo, vote.vector)
    end

    puts "#{i} - #{vote.casted_at} - bool_platform: #{"%.2f" % platform}, pos-neg: #{tot_pos}-#{tot_neg} (#{"%.2f" % (saldo.to_f/(tot_pos+tot_neg))}), last pivot at: #{last_pivot or '<never>'}"

    prev_time, prev_saldo = vote.casted_at, saldo
  end
  platform
end

# platform calculation for float vote vectors
def float_platform(votes)
  tot_pos = tot_neg = saldo = platform = prev_saldo = 0
  prev_time = last_pivot = nil

  votes.each_with_index do |vote|
    saldo += vote.vector
    tot_pos += vote.vector if vote.vector > 0
    tot_neg -= vote.vector if vote.vector < 0

    if saldo == 0 or prev_time.nil?
      platform = 0
    else
      duration = vote.casted_at - prev_time
      if (prev_saldo <=> 0) == (saldo <=> 0)  # if we do not cross (or touch) the time axis
        platform += calculate_surface(duration, prev_saldo, vote.vector)
      else
        duration_till_crossing = -prev_saldo / ((saldo - prev_saldo) / duration)
        platform = 0.5 * saldo * (duration - duration_till_crossing)
      end
    end

    puts "#{i} - #{vote.casted_at} - float_platform: #{"%.2f" % platform}, pos-neg: #{tot_pos}-#{tot_neg} (#{"%.2f" % (saldo.to_f/(tot_pos+tot_neg))}), last pivot at: #{last_pivot or '<never>'}"

    prev_time, prev_saldo = vote.casted_at, saldo
  end
  platform
end


class Time
  def to_s
    self.strftime "%Y.%M.%d %H:%M:%S"
  end
end

# prepare some votes
votes = []
now = Time.now
t = now - 200 * 60*60*24
count = 0
begin
  t += rand * 12 * 60*60
  v  = (rand - 0.8 + count*0.007) <=> 0
  votes << Vote.new(v, t)
  count += 1
end until t > now

bool_platform votes
