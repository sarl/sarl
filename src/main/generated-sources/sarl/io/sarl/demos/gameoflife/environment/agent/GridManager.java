package io.sarl.demos.gameoflife.environment.agent;

import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Capacity;
import java.util.List;
import java.util.UUID;
import org.eclipse.xtext.xbase.lib.Pair;

/**
 * @author Jérôme Boulmier
 */
@SarlSpecification("0.4")
@SuppressWarnings("all")
public interface GridManager extends Capacity {
  /**
   * Replies the agents perceptions
   */
  public abstract List<List<Pair<UUID, Boolean>>> getPerceptions();
  
  /**
   * Replies the width of the grid.
   */
  public abstract int getWidth();
  
  /**
   * Replies the height of the grid.
   */
  public abstract int getHeight();
}
