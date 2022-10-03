import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SarlSpecification("0.12")
@SarlElementType(15)
@SuppressWarnings("all")
public class event4 extends event3 {
  public int subsubid = 2;
  
  @SyntheticMember
  public event4(final Integer n) {
    super(n);
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
    event4 other = (event4) obj;
    if (other.subsubid != this.subsubid)
      return false;
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    final int prime = 31;
    result = prime * result + Integer.hashCode(this.subsubid);
    return result;
  }
  
  /**
   * Returns a String representation of the event4 event's attributes only.
   */
  @SyntheticMember
  @Pure
  protected void toString(final ToStringBuilder builder) {
    super.toString(builder);
    builder.add("subsubid", this.subsubid);
  }
  
  @SyntheticMember
  private static final long serialVersionUID = -3352410635L;
}
