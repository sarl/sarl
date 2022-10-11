import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class ConsoleErrorLogging extends Skill implements ErrorLogging {
  public void info(final String text) {
    System.out.println(text);
  }
  
  public void debug(final String text) {
    System.err.println(text);
  }
  
  public void error(final String text) {
    System.err.println(text);
  }
  
  @SyntheticMember
  public ConsoleErrorLogging() {
    super();
  }
  
  @SyntheticMember
  public ConsoleErrorLogging(final Agent agent) {
    super(agent);
  }
}
