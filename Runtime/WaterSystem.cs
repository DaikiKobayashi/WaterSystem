using System.IO;
using UnityEditor;
using UnityEngine;
using KDaili.WaterSystem.Data;
using UnityEngine.Rendering;

namespace KDaili.WaterSystem
{
    [ExecuteAlways]
    public class WaterSystem : MonoBehaviour
    {
        private static WaterSystem I;
        public static WaterSystem Instance
        {
            get
            {
                if (I == null)
                    I = (WaterSystem)FindObjectOfType(typeof(WaterSystem));
                return I;
            }
        }

        // サンプリングテクスチャセットパス ShaderGlobalName
        private static readonly int WaterColorTexture = Shader.PropertyToID("_WaterColorTexture");

        ///ShaderPropertyName///
        private readonly int Shader_NormalScale = Shader.PropertyToID("_NormalScale");
        private readonly int Shader_NormalTiling = Shader.PropertyToID("_NormalTiling");
        private readonly int Shader_NormalScrollSpeed = Shader.PropertyToID("_NormalTextureScrollSpeed");
        private readonly int Shader_DistortionPower = Shader.PropertyToID("_DistortionPower");
        private readonly int Shader_WaterColorBlend = Shader.PropertyToID("_WaterColorBlend");
        private readonly int Shader_Smoothness = Shader.PropertyToID("_Smoothness");
        private readonly int Shader_DepthDistance = Shader.PropertyToID("_DepthDistance");

        private Texture2D m_WaterGradientTexture;   

        [SerializeField]
        public WaterParameter waterParameter;
        [SerializeField]
        private WaterResources resources;

        private void OnEnable()
        {
            // コールバックに追加
            RenderPipelineManager.beginCameraRendering += RenderingWaterMesh;
            Init();
        }

        private void OnDisable()
        {
            // コールバックから削除
            RenderPipelineManager.beginCameraRendering -= RenderingWaterMesh;
        }

        public void Init()
        {
            AppendShaderPropertys();

            m_WaterGradientTexture = CreateGradientTexture2D(waterParameter._WaterGradientColor);
            SetGradientTexture(m_WaterGradientTexture);
        }

        /// <summary>
        /// メッシュレンダリング
        /// </summary>
        private void RenderingWaterMesh(ScriptableRenderContext src, Camera camera)
        {
            if (camera.cameraType == CameraType.Preview) return;

            var roll = camera.transform.localEulerAngles.z;

            // Water matrix
            const float quantizeValue = 6.25f;
            const float forwards = 10f;
            const float yOffset = -0.25f;

            var pos = camera.transform.TransformPoint(Vector3.forward * forwards);
            pos.y = yOffset;
            pos.x = quantizeValue * (int)(pos.x / quantizeValue);
            pos.z = quantizeValue * (int)(pos.z / quantizeValue);

            var matrix = Matrix4x4.TRS(pos + transform.position, Quaternion.identity, transform.localScale);

            foreach (var mesh in resources.defaultWaterMeshes)
            {
                Graphics.DrawMesh(
                    mesh,
                    matrix,
                    resources.defaultSeaMaterial,
                    gameObject.layer,
                    camera,
                    0,
                    null,
                    ShadowCastingMode.Off,
                    true,
                    null,
                    LightProbeUsage.Off,
                    null);
            }
        }

        /// <summary>
        /// シェーダーパラメーター更新
        /// </summary>
        public void AppendShaderPropertys()
        {
            // nullチェック
            if (resources == null || resources.defaultSeaMaterial == null || waterParameter == null)
                return;

            var t = resources.defaultSeaMaterial;
            t.SetFloat(Shader_NormalScale, waterParameter.m_NormalScale);
            t.SetVector(Shader_NormalTiling, waterParameter.m_NormalTiling);
            t.SetVector(Shader_NormalScrollSpeed, waterParameter.m_NormalScrollSpeed);
            t.SetFloat(Shader_DistortionPower, waterParameter.m_DistortionPower);
            t.SetFloat(Shader_WaterColorBlend, waterParameter.m_WaterColorBlend);
            t.SetFloat(Shader_Smoothness, waterParameter.m_Smoothness);
            t.SetFloat(Shader_DepthDistance, waterParameter.m_DepthDistance);
        }




















        public void WriteGradientTexture(string filePath)
        {
            var texture = m_WaterGradientTexture;

            // PNGのバイト情報に変更
            byte[] pngData = texture.EncodeToPNG();
            File.WriteAllBytes(filePath, pngData);

            // メモリリーク防止
            Object.DestroyImmediate(texture);
            AssetDatabase.Refresh();
        }

        /// <summary>
        /// Convert Gradient into Texture2D
        /// </summary>
        private Texture2D CreateGradientTexture2D(Gradient gradient)
        {
            var texture = new Texture2D(1024, 1, TextureFormat.RGBA32, false);
            for (var h = 0; h < texture.height; h++)
            {
                for (var w = 0; w < texture.width; w++)
                {
                    texture.SetPixel(w, h, gradient.Evaluate((float)w / texture.width));
                }
            }
            // テクスチャを上書き
            texture.Apply();
            return texture;
        }

        private void SetGradientTexture(Texture2D texture)
        {
            // テクスチャの設定を変更
            texture.wrapMode = TextureWrapMode.Clamp;
            // テクスチャをグローバル変数にセット
            Shader.SetGlobalTexture(WaterColorTexture, texture);
        }
    }
}