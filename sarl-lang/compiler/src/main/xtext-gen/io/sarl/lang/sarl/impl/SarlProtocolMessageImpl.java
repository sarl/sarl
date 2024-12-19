/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlProtocolMessage;
import io.sarl.lang.sarl.SarlProtocolParameter;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.impl.XtendMemberImplCustom;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Protocol Message</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolMessageImpl#getFrom <em>From</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolMessageImpl#getTo <em>To</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolMessageImpl#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolMessageImpl#getParameters <em>Parameters</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SarlProtocolMessageImpl extends XtendMemberImplCustom implements SarlProtocolMessage
{
	/**
	 * The default value of the '{@link #getFrom() <em>From</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFrom()
	 * @generated
	 * @ordered
	 */
	protected static final String FROM_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFrom() <em>From</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFrom()
	 * @generated
	 * @ordered
	 */
	protected String from = FROM_EDEFAULT;

	/**
	 * The default value of the '{@link #getTo() <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTo()
	 * @generated
	 * @ordered
	 */
	protected static final String TO_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTo() <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTo()
	 * @generated
	 * @ordered
	 */
	protected String to = TO_EDEFAULT;

	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getParameters() <em>Parameters</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getParameters()
	 * @generated
	 * @ordered
	 */
	protected EList<SarlProtocolParameter> parameters;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlProtocolMessageImpl()
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
		return SarlPackage.Literals.SARL_PROTOCOL_MESSAGE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getFrom()
	{
		return from;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setFrom(String newFrom)
	{
		String oldFrom = from;
		from = newFrom;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_PROTOCOL_MESSAGE__FROM, oldFrom, from));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getTo()
	{
		return to;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTo(String newTo)
	{
		String oldTo = to;
		to = newTo;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_PROTOCOL_MESSAGE__TO, oldTo, to));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getName()
	{
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setName(String newName)
	{
		String oldName = name;
		name = newName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_PROTOCOL_MESSAGE__NAME, oldName, name));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<SarlProtocolParameter> getParameters()
	{
		if (parameters == null)
		{
			parameters = new EObjectContainmentEList<SarlProtocolParameter>(SarlProtocolParameter.class, this, SarlPackage.SARL_PROTOCOL_MESSAGE__PARAMETERS);
		}
		return parameters;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_PROTOCOL_MESSAGE__PARAMETERS:
				return ((InternalEList<?>)getParameters()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
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
			case SarlPackage.SARL_PROTOCOL_MESSAGE__FROM:
				return getFrom();
			case SarlPackage.SARL_PROTOCOL_MESSAGE__TO:
				return getTo();
			case SarlPackage.SARL_PROTOCOL_MESSAGE__NAME:
				return getName();
			case SarlPackage.SARL_PROTOCOL_MESSAGE__PARAMETERS:
				return getParameters();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_PROTOCOL_MESSAGE__FROM:
				setFrom((String)newValue);
				return;
			case SarlPackage.SARL_PROTOCOL_MESSAGE__TO:
				setTo((String)newValue);
				return;
			case SarlPackage.SARL_PROTOCOL_MESSAGE__NAME:
				setName((String)newValue);
				return;
			case SarlPackage.SARL_PROTOCOL_MESSAGE__PARAMETERS:
				getParameters().clear();
				getParameters().addAll((Collection<? extends SarlProtocolParameter>)newValue);
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
			case SarlPackage.SARL_PROTOCOL_MESSAGE__FROM:
				setFrom(FROM_EDEFAULT);
				return;
			case SarlPackage.SARL_PROTOCOL_MESSAGE__TO:
				setTo(TO_EDEFAULT);
				return;
			case SarlPackage.SARL_PROTOCOL_MESSAGE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case SarlPackage.SARL_PROTOCOL_MESSAGE__PARAMETERS:
				getParameters().clear();
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
			case SarlPackage.SARL_PROTOCOL_MESSAGE__FROM:
				return FROM_EDEFAULT == null ? from != null : !FROM_EDEFAULT.equals(from);
			case SarlPackage.SARL_PROTOCOL_MESSAGE__TO:
				return TO_EDEFAULT == null ? to != null : !TO_EDEFAULT.equals(to);
			case SarlPackage.SARL_PROTOCOL_MESSAGE__NAME:
				return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
			case SarlPackage.SARL_PROTOCOL_MESSAGE__PARAMETERS:
				return parameters != null && !parameters.isEmpty();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString()
	{
		if (eIsProxy()) return super.toString();

		StringBuilder result = new StringBuilder(super.toString());
		result.append(" (from: ");
		result.append(from);
		result.append(", to: ");
		result.append(to);
		result.append(", name: ");
		result.append(name);
		result.append(')');
		return result.toString();
	}

} //SarlProtocolMessageImpl
