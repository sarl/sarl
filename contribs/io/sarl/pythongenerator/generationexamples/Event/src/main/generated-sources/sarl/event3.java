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
public class event3 extends Event {
  public final Integer initvalue;
  
  public int subid = 0;
  
  public event3(final Integer n) {
    this.initvalue = n;
    this.subid++;
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
    event3 other = (event3) obj;
    if (other.initvalue == null) {
      if (this.initvalue != null)
        return false;
    } else if (this.initvalue == null)
      return false;
    if (other.initvalue != null && other.initvalue.intValue() != this.initvalue.intValue())
      return false;
    if (other.subid != this.subid)
      return false;
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    final int prime = 31;
    result = prime * result + Objects.hashCode(this.initvalue);
    result = prime * result + Integer.hashCode(this.subid);
    return result;
  }
  
  /**
   * Returns a String representation of the event3 event's attributes only.
   */
  @SyntheticMember
  @Pure
  protected void toString(final ToStringBuilder builder) {
    super.toString(builder);
    builder.add("initvalue", this.initvalue);
    builder.add("subid", this.subid);
  }
}
