import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class MyLogging extends Skill implements Logging {
  public void info(final String text) {
    this.output(text);
  }
  
  public void debug(final String text) {
    this.output(text);
  }
  
  public void output(final String t) {
    System.err.println(t);
  }
  
  @SyntheticMember
  public MyLogging() {
    super();
  }
  
  @SyntheticMember
  public MyLogging(final Agent agent) {
    super(agent);
  }
}
