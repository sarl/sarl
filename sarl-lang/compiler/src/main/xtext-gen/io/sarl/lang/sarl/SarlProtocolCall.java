/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendMember;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Protocol Call</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolCall#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolCall#getParameters <em>Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolCall#getRoles <em>Roles</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolCall()
 * @model
 * @generated
 */
public interface SarlProtocolCall extends XtendMember
{
	/**
	 * Returns the value of the '<em><b>Name</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' containment reference.
	 * @see #setName(JvmTypeReference)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolCall_Name()
	 * @model containment="true"
	 * @generated
	 */
	JvmTypeReference getName();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlProtocolCall#getName <em>Name</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' containment reference.
	 * @see #getName()
	 * @generated
	 */
	void setName(JvmTypeReference value);

	/**
	 * Returns the value of the '<em><b>Parameters</b></em>' containment reference list.
	 * The list contents are of type {@link io.sarl.lang.sarl.SarlProtocolParameter}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Parameters</em>' containment reference list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolCall_Parameters()
	 * @model containment="true"
	 * @generated
	 */
	EList<SarlProtocolParameter> getParameters();

	/**
	 * Returns the value of the '<em><b>Roles</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Roles</em>' attribute list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolCall_Roles()
	 * @model
	 * @generated
	 */
	EList<String> getRoles();

} // SarlProtocolCall
