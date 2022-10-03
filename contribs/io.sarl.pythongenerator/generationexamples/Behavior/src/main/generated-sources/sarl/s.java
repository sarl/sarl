import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;
import org.eclipse.xtext.xbase.lib.InputOutput;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class s extends Skill implements c {
  public void dothing(final int thing) {
    InputOutput.<String>println("hello");
  }
  
  @SyntheticMember
  public s() {
    super();
  }
  
  @SyntheticMember
  public s(final Agent agent) {
    super(agent);
  }
}
