import io.sarl.core.OpenEventSpace;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import org.eclipse.xtext.xbase.lib.Pure;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class pongSkill extends Skill implements pongCapacity {
  private OpenEventSpace spc;
  
  public pongSkill(final OpenEventSpace pongSpace) {
    this.spc = pongSpace;
  }
  
  public void sendpong() {
    throw new Error("Unresolved compilation problems:"
      + "\nType mismatch: cannot convert from UUID to UUID"
      + "\nType mismatch: cannot convert from PongEvent to Event");
  }
  
  public void replyPong(final Event occ) {
    throw new Error("Unresolved compilation problems:"
      + "\nType mismatch: cannot convert from UUID to UUID"
      + "\nType mismatch: cannot convert from PongEvent to Event");
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    throw new Error("Unresolved compilation problems:"
      + "\nInvalid implemented type: \'pongCapacity\'. Only subtypes of \'io.sarl.lang.core.Capacity\' are allowed for \'pongSkill\'."
      + "\nInvalid implemented type: \'pongCapacity\'. Only subtypes of \'io.sarl.lang.core.Capacity\' are allowed for \'pongSkill\'."
      + "\nMissing implemented type \'io.sarl.lang.core.Capacity\' for \'pongSkill\'."
      + "\nMissing implemented type \'io.sarl.lang.core.Capacity\' for \'pongSkill\'.");
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    throw new Error("Unresolved compilation problems:"
      + "\nInvalid implemented type: \'pongCapacity\'. Only subtypes of \'io.sarl.lang.core.Capacity\' are allowed for \'pongSkill\'."
      + "\nMissing implemented type \'io.sarl.lang.core.Capacity\' for \'pongSkill\'.");
  }
}
