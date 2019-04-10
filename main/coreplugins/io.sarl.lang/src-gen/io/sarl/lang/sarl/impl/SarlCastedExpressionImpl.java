/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.xtext.common.types.JvmOperation;

import org.eclipse.xtext.xbase.XExpression;

import org.eclipse.xtext.xbase.impl.XCastedExpressionImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Casted Expression</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlCastedExpressionImpl#getFeature <em>Feature</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlCastedExpressionImpl#getReceiver <em>Receiver</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlCastedExpressionImpl#getArgument <em>Argument</em>}</li>
 * </ul>
 *
 * @since 0.9
 * @generated
 */
public class SarlCastedExpressionImpl extends XCastedExpressionImpl implements SarlCastedExpression
{
	/**
	 * The cached value of the '{@link #getFeature() <em>Feature</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFeature()
	 * @generated
	 * @ordered
	 */
	protected JvmOperation feature;

	/**
	 * The cached value of the '{@link #getReceiver() <em>Receiver</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getReceiver()
	 * @generated
	 * @ordered
	 */
	protected XExpression receiver;

	/**
	 * The cached value of the '{@link #getArgument() <em>Argument</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getArgument()
	 * @generated
	 * @ordered
	 */
	protected XExpression argument;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlCastedExpressionImpl()
	{
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass()
	{
		return SarlPackage.Literals.SARL_CASTED_EXPRESSION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public JvmOperation getFeature()
	{
		if (feature != null && feature.eIsProxy())
		{
			InternalEObject oldFeature = (InternalEObject)feature;
			feature = (JvmOperation)eResolveProxy(oldFeature);
			if (feature != oldFeature)
			{
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, SarlPackage.SARL_CASTED_EXPRESSION__FEATURE, oldFeature, feature));
			}
		}
		return feature;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public JvmOperation basicGetFeature()
	{
		return feature;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setFeature(JvmOperation newFeature)
	{
		JvmOperation oldFeature = feature;
		feature = newFeature;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_CASTED_EXPRESSION__FEATURE, oldFeature, feature));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public XExpression getReceiver()
	{
		if (receiver != null && receiver.eIsProxy())
		{
			InternalEObject oldReceiver = (InternalEObject)receiver;
			receiver = (XExpression)eResolveProxy(oldReceiver);
			if (receiver != oldReceiver)
			{
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, SarlPackage.SARL_CASTED_EXPRESSION__RECEIVER, oldReceiver, receiver));
			}
		}
		return receiver;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XExpression basicGetReceiver()
	{
		return receiver;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setReceiver(XExpression newReceiver)
	{
		XExpression oldReceiver = receiver;
		receiver = newReceiver;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_CASTED_EXPRESSION__RECEIVER, oldReceiver, receiver));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public XExpression getArgument()
	{
		if (argument != null && argument.eIsProxy())
		{
			InternalEObject oldArgument = (InternalEObject)argument;
			argument = (XExpression)eResolveProxy(oldArgument);
			if (argument != oldArgument)
			{
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, SarlPackage.SARL_CASTED_EXPRESSION__ARGUMENT, oldArgument, argument));
			}
		}
		return argument;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XExpression basicGetArgument()
	{
		return argument;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setArgument(XExpression newArgument)
	{
		XExpression oldArgument = argument;
		argument = newArgument;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_CASTED_EXPRESSION__ARGUMENT, oldArgument, argument));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CASTED_EXPRESSION__FEATURE:
				if (resolve) return getFeature();
				return basicGetFeature();
			case SarlPackage.SARL_CASTED_EXPRESSION__RECEIVER:
				if (resolve) return getReceiver();
				return basicGetReceiver();
			case SarlPackage.SARL_CASTED_EXPRESSION__ARGUMENT:
				if (resolve) return getArgument();
				return basicGetArgument();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CASTED_EXPRESSION__FEATURE:
				setFeature((JvmOperation)newValue);
				return;
			case SarlPackage.SARL_CASTED_EXPRESSION__RECEIVER:
				setReceiver((XExpression)newValue);
				return;
			case SarlPackage.SARL_CASTED_EXPRESSION__ARGUMENT:
				setArgument((XExpression)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CASTED_EXPRESSION__FEATURE:
				setFeature((JvmOperation)null);
				return;
			case SarlPackage.SARL_CASTED_EXPRESSION__RECEIVER:
				setReceiver((XExpression)null);
				return;
			case SarlPackage.SARL_CASTED_EXPRESSION__ARGUMENT:
				setArgument((XExpression)null);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CASTED_EXPRESSION__FEATURE:
				return feature != null;
			case SarlPackage.SARL_CASTED_EXPRESSION__RECEIVER:
				return receiver != null;
			case SarlPackage.SARL_CASTED_EXPRESSION__ARGUMENT:
				return argument != null;
		}
		return super.eIsSet(featureID);
	}

} //SarlCastedExpressionImpl
