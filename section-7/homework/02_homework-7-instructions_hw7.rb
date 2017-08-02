# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  def equal_to_fzero(r)
    real_close(0.0, r)
  end

  def point_equal_to_origin(p)
    real_close_point(0.0, 0.0, p.x, p.y)
  end

  def point_in_line(p, l)
    equal_to_fzero(l.m * p.x + l.b - p.y)
  end

  def point_in_vline(p, l)
    real_close(p.x, l.x)
  end


  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end

  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    if equal_to_fzero(dx) and equal_to_fzero(dy)
      self
    else
      Point.new(x + dx, y + dy)
    end
  end
  def intersect other
    other.intersectPoint self
  end
  def intersectPoint p
    if real_close_point(x, y, p.x, p.y)
      self
    else
      NoPoints.new
    end
  end
  def intersectLine line
    if point_in_line(self, line)
      self
    else
      NoPoints.new
    end
  end
  def intersectVerticalLine vline
    if point_in_vline(self, vline)
      self
    else
      NoPoints.new
    end
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    if ((real_close(x, seg.x1) or seg.x1 < x) and (real_close(x, seg.x2) or seg.x2 > x)) and ((real_close(y, seg.y1) or seg.y1 < y) and (real_close(y, seg.y2) or seg.y2 > y))
      self
    else
      NoPoints.new
    end
  end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end

  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    delta_b = dy - m * dx
    if equal_to_fzero(delta_b)
      self
    else
      Line.new(m, b + delta_b)
    end
  end
  def intersect other
    other.intersectLine self
  end
  def intersectPoint p
    if equal_to_fzero(m * p.x + b - p.y)
      p
    else
      NoPoints.new
    end
  end
  def intersectLine line
    if real_close(m, line.m)
      if real_close(b, line.b)
        self
      else
        NoPoints.new
      end
    else
      x = (line.b - b).to_f / (m - line.m)
      y = m * x + b
      Point.new(x, y)
    end
  end

  def intersectVerticalLine vline
    Point.new(vline.x, m * vline.x + b)
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end

  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    if equal_to_fzero(dx)
      self
    else
      VerticalLine.new(x + dx)
    end
  end
  def intersect other
    other.intersectVerticalLine self
  end
  def intersectPoint p
    if equal_to_fzero(x - p.x)
      p
    else
      NoPoints.new
    end
  end
  def intersectLine line
    Point.new(x, line.m * x + b)
  end

  def intersectVerticalLine vline
    if real_close(x, vline.x)
      self
    else
      NoPoints.new
    end
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    if real_close(x1, x2)
      if real_close(y1, y2)
        Point.new(x1, y1)
      elsif y1 > y2
        LineSegment.new(x2, y2, x1, y1)
      end
    elsif x1 > x2
      LineSegment.new(x2, y2, x1, y1)
    else
      self
    end
  end
  def shift(dx,dy)
    if equal_to_fzero(dx) && equal_to_fzero(dy)
      self
    else
      LineSegment.new(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
    end
  end
  def intersect other
    other.intersectLineSegment self
  end
  def intersectPoint p
    p.intersectLineSegment(self)
  end
  def intersectLine line
    line.intersectLineSegment(self)
  end
  def intersectVerticalLine vline
    vline.intersectLineSegment(self)
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    if real_close(x1, seg.x1)
      # p1 same
      if real_close(y1, seg.y1)
        # p2 < seg.p2
        if x2 < seg.x2
          self
        # p2 > seg.p2
        else
          seg
        end
        # VSegLine
      elsif y1 < seg.y1
        if y2 < seg.y2
          self
        else
          seg
        end
      else
        seg.intersectWithSegmentAsLineResult(self)
      end
    elsif x1 < seg.x1
      if real_close(x2, seg.x1)
        Point.new(x2, y2)
      elsif x2 < seg.x1
        NoPoints.new
      else
        if x2 > seg.x2
          seg
        else
          LineSegment.new(seg.x1, seg.y1, x2, y2)
        end
      end
    else
      seg.intersectWithSegmentAsLineResult(self)
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
end
