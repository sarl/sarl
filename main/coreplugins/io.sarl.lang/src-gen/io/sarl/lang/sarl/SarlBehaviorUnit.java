/**
 */
package io.sarl.lang.sarl;

import org.eclipse.xtend.core.xtend.XtendMember;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Behavior Unit</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlBehaviorUnit#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlBehaviorUnit#getGuard <em>Guard</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlBehaviorUnit#getExpression <em>Expression</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlBehaviorUnit()
 * @model
 * @generated
 */
public interface SarlBehaviorUnit extends XtendMember
{
	/**
	 * Returns the value of the '<em><b>Name</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' containment reference.
	 * @see #setName(JvmParameterizedTypeReference)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlBehaviorUnit_Name()
	 * @model containment="true"
	 * @generated
	 */
	JvmParameterizedTypeReference getName();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlBehaviorUnit#getName <em>Name</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' containment reference.
	 * @see #getName()
	 * @generated
	 */
	void setName(JvmParameterizedTypeReference value);

	/**
	 * Returns the value of the '<em><b>Guard</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Guard</em>' containment reference.
	 * @see #setGuard(XExpression)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlBehaviorUnit_Guard()
	 * @model containment="true"
	 * @generated
	 */
	XExpression getGuard();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlBehaviorUnit#getGuard <em>Guard</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Guard</em>' containment reference.
	 * @see #getGuard()
	 * @generated
	 */
	void setGuard(XExpression value);

	/**
	 * Returns the value of the '<em><b>Expression</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Expression</em>' containment reference.
	 * @see #setExpression(XExpression)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlBehaviorUnit_Expression()
	 * @model containment="true"
	 * @generated
	 */
	XExpression getExpression();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlBehaviorUnit#getExpression <em>Expression</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Expression</em>' containment reference.
	 * @see #getExpression()
	 * @generated
	 */
	void setExpression(XExpression value);

} // SarlBehaviorUnit
