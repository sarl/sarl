/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Capacity Uses</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.CapacityUses#getCapacitiesUsed <em>Capacities Used</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getCapacityUses()
 * @model
 * @generated
 */
public interface CapacityUses extends AgentFeature, BehaviorFeature, SkillFeature
{
  /**
   * Returns the value of the '<em><b>Capacities Used</b></em>' reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.Capacity}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Capacities Used</em>' reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Capacities Used</em>' reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getCapacityUses_CapacitiesUsed()
   * @model
   * @generated
   */
  EList<Capacity> getCapacitiesUsed();

} // CapacityUses
