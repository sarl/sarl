/**
 */
package io.sarl.lang.sarl;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Assert Expression</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * @since 0.6
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlAssertExpression#getCondition <em>Condition</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlAssertExpression#getMessage <em>Message</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlAssertExpression#isIsStatic <em>Is Static</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlAssertExpression()
 * @model
 * @generated
 */
public interface SarlAssertExpression extends XExpression
{
	/**
	 * Returns the value of the '<em><b>Condition</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Condition</em>' containment reference.
	 * @see #setCondition(XExpression)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlAssertExpression_Condition()
	 * @model containment="true"
	 * @generated
	 */
	XExpression getCondition();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlAssertExpression#getCondition <em>Condition</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Condition</em>' containment reference.
	 * @see #getCondition()
	 * @generated
	 */
	void setCondition(XExpression value);

	/**
	 * Returns the value of the '<em><b>Message</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Message</em>' attribute.
	 * @see #setMessage(String)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlAssertExpression_Message()
	 * @model
	 * @generated
	 */
	String getMessage();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlAssertExpression#getMessage <em>Message</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Message</em>' attribute.
	 * @see #getMessage()
	 * @generated
	 */
	void setMessage(String value);

	/**
	 * Returns the value of the '<em><b>Is Static</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Static</em>' attribute.
	 * @see #setIsStatic(boolean)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlAssertExpression_IsStatic()
	 * @model
	 * @generated
	 */
	boolean isIsStatic();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlAssertExpression#isIsStatic <em>Is Static</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Static</em>' attribute.
	 * @see #isIsStatic()
	 * @generated
	 */
	void setIsStatic(boolean value);

} // SarlAssertExpression
