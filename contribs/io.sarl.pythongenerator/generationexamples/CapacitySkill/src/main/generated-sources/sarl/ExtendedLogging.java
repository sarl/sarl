import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Capacity;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class ExtendedLogging extends StandardJavaLogging implements Capacity {
  public void info(final String text) {
    super.info(("INFO: " + text));
  }
  
  @SyntheticMember
  public ExtendedLogging() {
    super();
  }
  
  @SyntheticMember
  public ExtendedLogging(final Agent agent) {
    super(agent);
  }
}
