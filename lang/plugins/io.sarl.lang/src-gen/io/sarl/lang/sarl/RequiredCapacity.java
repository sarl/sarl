/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Required Capacity</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.RequiredCapacity#getRequiredCapacities <em>Required Capacities</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getRequiredCapacity()
 * @model
 * @generated
 */
public interface RequiredCapacity extends Feature
{
  /**
   * Returns the value of the '<em><b>Required Capacities</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Required Capacities</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Required Capacities</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getRequiredCapacity_RequiredCapacities()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getRequiredCapacities();

} // RequiredCapacity
