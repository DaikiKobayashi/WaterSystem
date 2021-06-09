using JetBrains.Annotations;
using System.Collections;
using System.Collections.Generic;
using Unity.Collections;
using UnityEngine;

namespace KDaili.WaterSystem.Data
{
    [System.Serializable, CreateAssetMenu(fileName = "WaterColorData", menuName = "WaterData/Data", order = 0)]
    public class WaterParameter : ScriptableObject
    {
        // GradientColor
        public Gradient _WaterGradientColor;

        public float m_NormalScale = 0.5F;
        public Vector4 m_NormalTiling = new Vector4(1F, 1F, 1F, 1F);
        public Vector4 m_NormalScrollSpeed = new Vector4(-0.03F, 0F, 0.044F, 0.04F);
        public float m_DistortionPower = 0.05F;
        public float m_WaterColorBlend = 1;
        public float m_Smoothness = 0.6F;
        public float m_DepthDistance = 10F;
    }
}