/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Protocol</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocol#getExtends <em>Extends</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocol()
 * @model
 * @generated
 */
public interface SarlProtocol extends XtendTypeDeclaration
{
	/**
	 * Returns the value of the '<em><b>Extends</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Extends</em>' containment reference.
	 * @see #setExtends(JvmParameterizedTypeReference)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocol_Extends()
	 * @model containment="true"
	 * @generated
	 */
	JvmParameterizedTypeReference getExtends();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlProtocol#getExtends <em>Extends</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Extends</em>' containment reference.
	 * @see #getExtends()
	 * @generated
	 */
	void setExtends(JvmParameterizedTypeReference value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	boolean isAbstract();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation" unique="false"
	 * @generated
	 */
	EList<SarlProtocolRole> getRoles();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation" unique="false"
	 * @generated
	 */
	EList<String> getRoleNames();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation" unique="false"
	 * @generated
	 */
	EList<SarlProtocolMessage> getMessages();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation" unique="false"
	 * @generated
	 */
	EList<SarlProtocolParameter> getParameters();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation" unique="false"
	 * @generated
	 */
	EList<String> getParameterNames();

} // SarlProtocol
