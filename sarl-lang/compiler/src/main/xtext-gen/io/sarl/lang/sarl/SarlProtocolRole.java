/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendMember;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Protocol Role</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolRole#getNames <em>Names</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolRole()
 * @model
 * @generated
 */
public interface SarlProtocolRole extends XtendMember
{
	/**
	 * Returns the value of the '<em><b>Names</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Names</em>' attribute list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolRole_Names()
	 * @model unique="false"
	 * @generated
	 */
	EList<String> getNames();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model
	 * @generated
	 */
	void setName(String uniqueName);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	SarlProtocol getProtocol();

} // SarlProtocolRole
