import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class ConsoleLogging extends Skill implements Logging {
  public void info(final String text) {
    System.out.println(text);
  }
  
  public void debug(final String text) {
    System.err.println(text);
  }
  
  @SyntheticMember
  public ConsoleLogging() {
    super();
  }
  
  @SyntheticMember
  public ConsoleLogging(final Agent agent) {
    super(agent);
  }
}
