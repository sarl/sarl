import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;

@SarlSpecification("0.12")
@SarlElementType(15)
@SuppressWarnings("all")
public class event1 extends Event {
  @SyntheticMember
  public event1() {
    super();
  }
  
  @SyntheticMember
  public event1(final Address source) {
    super(source);
  }
}
