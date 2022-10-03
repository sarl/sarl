import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class MySkill extends Skill implements MyCapacity {
  public void myfunction() {
  }
  
  @SyntheticMember
  public MySkill() {
    super();
  }
  
  @SyntheticMember
  public MySkill(final Agent agent) {
    super(agent);
  }
}
