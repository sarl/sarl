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
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlRequiredCapacity#getCapacities <em>Capacities</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlRequiredCapacity()
 * @model
 * @generated
 */
public interface SarlRequiredCapacity extends XtendMember
{
	/**
	 * Returns the value of the '<em><b>Capacities</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Capacities</em>' containment reference list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlRequiredCapacity_Capacities()
	 * @model containment="true"
	 * @generated
	 */
	EList<JvmParameterizedTypeReference> getCapacities();

} // SarlRequiredCapacity
