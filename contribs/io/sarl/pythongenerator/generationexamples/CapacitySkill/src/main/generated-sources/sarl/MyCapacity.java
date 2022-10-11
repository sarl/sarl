import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.DefaultSkill;

@DefaultSkill(MySkill.class)
@FunctionalInterface
@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface MyCapacity extends Capacity {
  void myfunction();
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends MyCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements MyCapacity {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void myfunction() {
      try {
        ensureCallerInLocalThread();
        this.capacity.myfunction();
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
