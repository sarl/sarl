/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Skill</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getExtends <em>Extends</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getImplements <em>Implements</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill()
 * @model
 * @generated
 */
public interface SarlSkill extends XtendTypeDeclaration
{
	/**
	 * Returns the value of the '<em><b>Extends</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Extends</em>' containment reference.
	 * @see #setExtends(JvmParameterizedTypeReference)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_Extends()
	 * @model containment="true"
	 * @generated
	 */
	JvmParameterizedTypeReference getExtends();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlSkill#getExtends <em>Extends</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Extends</em>' containment reference.
	 * @see #getExtends()
	 * @generated
	 */
	void setExtends(JvmParameterizedTypeReference value);

	/**
	 * Returns the value of the '<em><b>Implements</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Implements</em>' containment reference list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_Implements()
	 * @model containment="true"
	 * @generated
	 */
	EList<JvmParameterizedTypeReference> getImplements();

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
	 * @model kind="operation"
	 * @generated
	 */
	boolean isStrictFloatingPoint();

} // SarlSkill
