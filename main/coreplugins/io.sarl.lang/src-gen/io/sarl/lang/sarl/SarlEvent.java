/**
 */
package io.sarl.lang.sarl;

import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlEvent#getExtends <em>Extends</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlEvent()
 * @model
 * @generated
 */
public interface SarlEvent extends XtendTypeDeclaration
{
	/**
	 * Returns the value of the '<em><b>Extends</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Extends</em>' containment reference.
	 * @see #setExtends(JvmParameterizedTypeReference)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlEvent_Extends()
	 * @model containment="true"
	 * @generated
	 */
	JvmParameterizedTypeReference getExtends();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlEvent#getExtends <em>Extends</em>}' containment reference.
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

} // SarlEvent
