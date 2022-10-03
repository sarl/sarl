import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;

@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface LogReader extends Capacity {
  int open(final String filename);
  
  void info(final String t);
  
  void close(final int fid);
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends LogReader> extends Capacity.ContextAwareCapacityWrapper<C> implements LogReader {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public int open(final String filename) {
      try {
        ensureCallerInLocalThread();
        return this.capacity.open(filename);
      } finally {
        resetCallerInLocalThread();
      }
    }
    
    public void info(final String t) {
      try {
        ensureCallerInLocalThread();
        this.capacity.info(t);
      } finally {
        resetCallerInLocalThread();
      }
    }
    
    public void close(final int fid) {
      try {
        ensureCallerInLocalThread();
        this.capacity.close(fid);
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
