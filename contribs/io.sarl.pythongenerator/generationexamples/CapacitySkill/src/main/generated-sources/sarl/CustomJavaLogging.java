import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Skill;
import java.util.logging.Logger;
import org.eclipse.xtext.xbase.lib.Pure;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class CustomJavaLogging extends Skill implements Logging {
  private final Logger logger;
  
  public CustomJavaLogging(final Logger l) {
    super();
    this.logger = l;
  }
  
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
}
