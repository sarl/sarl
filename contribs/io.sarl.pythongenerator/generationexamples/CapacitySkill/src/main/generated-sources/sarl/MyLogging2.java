import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class MyLogging2 extends Skill implements Logging, LogReader {
  public void info(final String text) {
    System.out.println(text);
  }
  
  public void debug(final String text) {
    System.out.println(text);
  }
  
  public int open(final String filename) {
    return 0;
  }
  
  public void close(final int fid) {
  }
  
  @SyntheticMember
  public MyLogging2() {
    super();
  }
  
  @SyntheticMember
  public MyLogging2(final Agent agent) {
    super(agent);
  }
}
