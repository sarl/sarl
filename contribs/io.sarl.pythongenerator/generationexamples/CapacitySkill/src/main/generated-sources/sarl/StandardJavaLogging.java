import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;
import java.util.logging.Logger;
import org.eclipse.xtext.xbase.lib.Pure;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class StandardJavaLogging extends Skill implements Logging {
  private final Logger logger = Logger.getAnonymousLogger();
  
  public void info(final String text) {
    this.logger.info(text);
  }
  
  public void debug(final String text) {
    this.logger.fine(text);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    return result;
  }
  
  @SyntheticMember
  public StandardJavaLogging() {
    super();
  }
  
  @SyntheticMember
  public StandardJavaLogging(final Agent agent) {
    super(agent);
  }
}
