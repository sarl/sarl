/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendMember;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Required Capacity</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlRequiredCapacity#getRequiredCapacities <em>Required Capacities</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlRequiredCapacity()
 * @model
 * @generated
 */
public interface SarlRequiredCapacity extends XtendMember
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
   * @see io.sarl.lang.sarl.SarlPackage#getSarlRequiredCapacity_RequiredCapacities()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getRequiredCapacities();

} // SarlRequiredCapacity
