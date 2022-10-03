import Cap1;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;

@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface Cap3 extends Cap1, Cap2 {
  void action3();
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends Cap3> extends Cap1.ContextAwareCapacityWrapper<C> implements Cap3 {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void action3() {
      try {
        ensureCallerInLocalThread();
        this.capacity.action3();
      } finally {
        resetCallerInLocalThread();
      }
    }
    
    public void action2() {
      try {
        ensureCallerInLocalThread();
        this.capacity.action2();
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
