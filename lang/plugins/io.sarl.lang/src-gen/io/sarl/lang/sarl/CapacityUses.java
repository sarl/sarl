/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

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
public interface CapacityUses extends Feature
{
  /**
   * Returns the value of the '<em><b>Capacities Used</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Capacities Used</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Capacities Used</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getCapacityUses_CapacitiesUsed()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getCapacitiesUsed();

} // CapacityUses
