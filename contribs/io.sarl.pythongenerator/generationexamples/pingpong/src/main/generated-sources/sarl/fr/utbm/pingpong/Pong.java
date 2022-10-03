package fr.utbm.pingpong;

import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Event;
import java.util.Objects;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SarlSpecification("0.12")
@SarlElementType(15)
@SuppressWarnings("all")
public class Pong extends Event {
  public Integer value;
  
  public Pong(final Integer v) {
    this.value = v;
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    Pong other = (Pong) obj;
    if (other.value == null) {
      if (this.value != null)
        return false;
    } else if (this.value == null)
      return false;
    if (other.value != null && other.value.intValue() != this.value.intValue())
      return false;
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    final int prime = 31;
    result = prime * result + Objects.hashCode(this.value);
    return result;
  }
  
  /**
   * Returns a String representation of the Pong event's attributes only.
   */
  @SyntheticMember
  @Pure
  protected void toString(final ToStringBuilder builder) {
    super.toString(builder);
    builder.add("value", this.value);
  }
  
  @SyntheticMember
  private static final long serialVersionUID = -693299882L;
}
