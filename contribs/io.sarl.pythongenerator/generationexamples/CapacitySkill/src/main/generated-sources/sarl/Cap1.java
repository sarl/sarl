import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;

@FunctionalInterface
@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface Cap1 extends Capacity {
  void action1();
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends Cap1> extends Capacity.ContextAwareCapacityWrapper<C> implements Cap1 {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void action1() {
      try {
        ensureCallerInLocalThread();
        this.capacity.action1();
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
