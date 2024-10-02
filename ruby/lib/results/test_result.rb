class TestResult

  attr_reader :status

  def initialize(name, status, message = nil)
    @name = name
    @status = status
    @message = message
  end

  def pasado?
    @status == :pasado
  end

  def fallido?
    @status == :fallido
  end

  def exploto?
    @status == :exploto
  end

  def to_s
    case @status
    when :pasado
      "  #{@name} PASS"
    when :fallido
      "  #{@name}: #{@message} FAIL"
    when :exploto
      "  #{@name}:\n    #{@message}"
    end
  end
end