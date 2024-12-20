/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlProtocol;
import io.sarl.lang.sarl.SarlProtocolCall;
import io.sarl.lang.sarl.SarlProtocolParameter;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.impl.XtendMemberImplCustom;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Protocol Call</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolCallImpl#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolCallImpl#getParameters <em>Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlProtocolCallImpl#getRoles <em>Roles</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SarlProtocolCallImpl extends XtendMemberImplCustom implements SarlProtocolCall
{
	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected JvmTypeReference name;

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
	 * The cached value of the '{@link #getRoles() <em>Roles</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRoles()
	 * @generated
	 * @ordered
	 */
	protected EList<String> roles;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlProtocolCallImpl()
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
		return SarlPackage.Literals.SARL_PROTOCOL_CALL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public JvmTypeReference getName()
	{
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetName(JvmTypeReference newName, NotificationChain msgs)
	{
		JvmTypeReference oldName = name;
		name = newName;
		if (eNotificationRequired())
		{
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_PROTOCOL_CALL__NAME, oldName, newName);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setName(JvmTypeReference newName)
	{
		if (newName != name)
		{
			NotificationChain msgs = null;
			if (name != null)
				msgs = ((InternalEObject)name).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_PROTOCOL_CALL__NAME, null, msgs);
			if (newName != null)
				msgs = ((InternalEObject)newName).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_PROTOCOL_CALL__NAME, null, msgs);
			msgs = basicSetName(newName, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_PROTOCOL_CALL__NAME, newName, newName));
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
			parameters = new EObjectContainmentEList<SarlProtocolParameter>(SarlProtocolParameter.class, this, SarlPackage.SARL_PROTOCOL_CALL__PARAMETERS);
		}
		return parameters;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getRoles()
	{
		if (roles == null)
		{
			roles = new EDataTypeEList<String>(String.class, this, SarlPackage.SARL_PROTOCOL_CALL__ROLES);
		}
		return roles;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public SarlProtocol getProtocol()
	{
		// TODO: implement this method
		// Ensure that you remove @generated or mark it @generated NOT
		throw new UnsupportedOperationException();
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
			case SarlPackage.SARL_PROTOCOL_CALL__NAME:
				return basicSetName(null, msgs);
			case SarlPackage.SARL_PROTOCOL_CALL__PARAMETERS:
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
			case SarlPackage.SARL_PROTOCOL_CALL__NAME:
				return getName();
			case SarlPackage.SARL_PROTOCOL_CALL__PARAMETERS:
				return getParameters();
			case SarlPackage.SARL_PROTOCOL_CALL__ROLES:
				return getRoles();
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
			case SarlPackage.SARL_PROTOCOL_CALL__NAME:
				setName((JvmTypeReference)newValue);
				return;
			case SarlPackage.SARL_PROTOCOL_CALL__PARAMETERS:
				getParameters().clear();
				getParameters().addAll((Collection<? extends SarlProtocolParameter>)newValue);
				return;
			case SarlPackage.SARL_PROTOCOL_CALL__ROLES:
				getRoles().clear();
				getRoles().addAll((Collection<? extends String>)newValue);
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
			case SarlPackage.SARL_PROTOCOL_CALL__NAME:
				setName((JvmTypeReference)null);
				return;
			case SarlPackage.SARL_PROTOCOL_CALL__PARAMETERS:
				getParameters().clear();
				return;
			case SarlPackage.SARL_PROTOCOL_CALL__ROLES:
				getRoles().clear();
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
			case SarlPackage.SARL_PROTOCOL_CALL__NAME:
				return name != null;
			case SarlPackage.SARL_PROTOCOL_CALL__PARAMETERS:
				return parameters != null && !parameters.isEmpty();
			case SarlPackage.SARL_PROTOCOL_CALL__ROLES:
				return roles != null && !roles.isEmpty();
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
		result.append(" (roles: ");
		result.append(roles);
		result.append(')');
		return result.toString();
	}

} //SarlProtocolCallImpl
