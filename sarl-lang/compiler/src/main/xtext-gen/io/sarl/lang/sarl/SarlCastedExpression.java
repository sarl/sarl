/**
 */
package io.sarl.lang.sarl;

import org.eclipse.xtext.common.types.JvmOperation;

import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Casted Expression</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * @since 0.9
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlCastedExpression#getFeature <em>Feature</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlCastedExpression#getReceiver <em>Receiver</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlCastedExpression#getArgument <em>Argument</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlCastedExpression()
 * @model
 * @generated
 */
public interface SarlCastedExpression extends XCastedExpression
{
	/**
	 * Returns the value of the '<em><b>Feature</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Provide the feature to call in casting the value to target type.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Feature</em>' reference.
	 * @see #setFeature(JvmOperation)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlCastedExpression_Feature()
	 * @model
	 * @generated
	 */
	JvmOperation getFeature();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlCastedExpression#getFeature <em>Feature</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Feature</em>' reference.
	 * @see #getFeature()
	 * @generated
	 */
	void setFeature(JvmOperation value);

	/**
	 * Returns the value of the '<em><b>Receiver</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Provide the receiver for the call to the feature, or null if the default should be applied.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Receiver</em>' reference.
	 * @see #setReceiver(XExpression)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlCastedExpression_Receiver()
	 * @model
	 * @generated
	 */
	XExpression getReceiver();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlCastedExpression#getReceiver <em>Receiver</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Receiver</em>' reference.
	 * @see #getReceiver()
	 * @generated
	 */
	void setReceiver(XExpression value);

	/**
	 * Returns the value of the '<em><b>Argument</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * Provide the argument to pass to the feature, or null if the feature does not need to have argument.
	 * <!-- end-model-doc -->
	 * @return the value of the '<em>Argument</em>' reference.
	 * @see #setArgument(XExpression)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlCastedExpression_Argument()
	 * @model
	 * @generated
	 */
	XExpression getArgument();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlCastedExpression#getArgument <em>Argument</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Argument</em>' reference.
	 * @see #getArgument()
	 * @generated
	 */
	void setArgument(XExpression value);

} // SarlCastedExpression
